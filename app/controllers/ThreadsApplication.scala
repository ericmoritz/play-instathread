package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText, text}
import models.{Threads, Thread, Comment}
import play.api.libs.json._
import com.github.jsonldjava.utils.{JsonUtils}
import play.api.mvc.BodyParsers.parse
import play.api.mvc.BodyParsers.parse.DEFAULT_MAX_TEXT_LENGTH
import com.github.jsonldjava.utils.JsonUtils
import scala.collection.JavaConversions._
import play.api.Logger

object ThreadsApplication extends Controller {
  /************************************************************
   ** JSON writers
   ************************************************************/
  implicit var commentWrites = new Writes[Comment] {
    def writes(comment: Comment) = Json.obj(
      "@type" -> "Comment",
      "vocab:authorName" -> comment.authorName,
      "schema:dateCreated" -> comment.dateCreated,
      "vocab:markdown" -> comment.markdown
    )
  }

  implicit val threadWrites = new Writes[Thread] {
    def writes(thread: Thread) = Json.obj(
      "@type" -> "Thread",
      "hydra:member" -> thread.member.toSeq.sortBy(_.dateCreated)
    )
  }


  /************************************************************
   ** Forms
   ************************************************************/
  case class NewCommentRequest(authorName: String, markdown: String)

  def jsonNewCommentRequest(json: JsValue): Option[NewCommentRequest] =
    (json \ "markdown").asOpt[String].map { markdown => 
      NewCommentRequest(
        (json \ "authorName").asOpt[String].getOrElse(""),
        markdown
      )
    }

  /************************************************************
   ** body parsers
   ************************************************************/
  val jsonLdCompact = parse.using { request =>
    val context = Seq(
      "vocab" -> "http://play-instathread.herokuapp.com/assets/vocab/instathread.jsonld#",
      "authorName" -> "vocab:authorName",
      "markdown" -> "vocab:markdown"
    ).foldLeft(new java.util.HashMap[String, String]()) { (hm, kv) => 
      hm.put(kv._1, kv._2)
      hm
    }
    parsers.tolerantJsonLd(DEFAULT_MAX_TEXT_LENGTH)(context)
  }


  /************************************************************
   ** utilities
   ************************************************************/
  def defaultHeaders(request: Request[AnyContent])(cb: => Result): Result = {
    val docUrl = routes.Assets.at("vocab/instathread.jsonld").absoluteURL(request.secure)(request)
    cb.withHeaders(
      "Link" -> ("<" + docUrl + ">; rel=\"http://www.w3.org/ns/hydra/core#apiDocumentation\"")
    ).as("application/ld+json")
  }

  def context(request: Request[AnyContent]): JsObject = Json.obj(
    "@context" -> routes.Assets.at("contexts/instathread.jsonld").absoluteURL(request.secure)(request),
    "entrypoint" -> routes.ThreadsApplication.index().absoluteURL(request.secure)(request)
  )


  /************************************************************
   ** Actions
   ************************************************************/
  def index = Action { request =>
    defaultHeaders(request) {
      Ok(
        context(request) ++
          Json.obj(
            "@id" -> request.uri,
            "@type" -> "EntryPoint",
            "threads" -> routes.ThreadsApplication.create().absoluteURL(request.secure)(request)
          )
      )
    }
  }

  def create = Action(jsonLdCompact) { implicit request =>
    jsonNewCommentRequest(parsers.toJsValue(request.body)).map { newCommentRequest =>
        val thread = Threads.store(
          Thread(
            newCommentRequest.authorName,
            newCommentRequest.markdown
          )
        )
        Redirect(
          routes.ThreadsApplication.get(thread.id)
        )
    }.getOrElse(BadRequest("Invalid request"))
  }


  def get(id: String) = Action { request =>
    Threads.get(id).map { thread =>
      defaultHeaders(request) {
        Ok(
          context(request) ++
            Json.toJson(thread).as[JsObject] ++
            Json.obj("@id" -> routes.ThreadsApplication.get(thread.id).absoluteURL(request.secure)(request))
        )
      }
    }.getOrElse(NotFound)
  }

  def postComment(id: String) = Action(jsonLdCompact) { implicit request =>
    Threads.get(id).map { thread =>
      jsonNewCommentRequest(parsers.toJsValue(request.body)).map { comment =>
          Threads.store(
            Thread.post(
              thread, 
              Comment(comment.authorName, comment.markdown)
            )
          )
          Redirect(
            routes.ThreadsApplication.get(thread.id)
          )
      }.getOrElse(BadRequest("Invalid request"))
    }.getOrElse(NotFound)
  }

}
