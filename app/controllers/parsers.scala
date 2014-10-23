package controllers

import play.api.mvc.BodyParser
import play.api.mvc.BodyParsers.parse
import play.api.libs.json._
import com.github.jsonldjava.utils.JsonUtils
import com.github.jsonldjava.core.{JsonLdProcessor, JsonLdOptions}
import play.api.libs.iteratee._
import play.api.mvc.Results
import java.io.ByteArrayInputStream
import scala.collection.JavaConverters._

object parsers {

  def toJsValue(obj: Object): JsValue = { 
    val result = obj match {
      case m: java.util.Map[java.lang.String, Object] => m.asScala.foldLeft(Json.obj()) { (acc, x) => 
        acc + (x._1.asInstanceOf[String], toJsValue(x._2))
      }
      case a: java.util.List[Object] => a.asScala.foldLeft(Json.arr()) { (acc, x) => 
        acc :+ toJsValue(x)
      }
      case s: java.lang.String => JsString(s)
      case n: java.lang.Double => JsNumber(new java.math.BigDecimal(n))
      case b: java.lang.Boolean => JsBoolean(b)
      case _ => JsNull
    }
    result
  }

  def tolerantJsonLd(maxLength: Int)(context: Object): BodyParser[Object] = BodyParser("ld+json, maxLength=" + maxLength) { request =>
    import Execution.Implicits.trampoline

    Traversable.takeUpTo[Array[Byte]](maxLength)
      .transform(Iteratee.consume[Array[Byte]]())
      .flatMap(Iteratee.eofOrElse(Results.EntityTooLarge)).map {
      case Right(bytes) => 
        Right(JsonLdProcessor.compact(
          JsonUtils.fromInputStream(new ByteArrayInputStream(bytes)),
          context,
          new JsonLdOptions()
        ))
      case Left(status) => Left(status)
    }
  }
}
