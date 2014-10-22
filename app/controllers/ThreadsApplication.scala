package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText, text}
import models.{Threads, Thread, Comment}
import play.api.libs.json._

case class NewThreadRequest(authorName: String, markdown: String)
case class NewCommentRequest(authorName: String, markdown: String)


object ThreadsApplication extends Controller {
  implicit var commentWrites = new Writes[Comment] {
    def writes(comment: Comment) = Json.obj(
      "it:authorName" -> comment.authorName,
      "schema:dateCreated" -> comment.dateCreated,
      "it:markdown" -> comment.markdown
    )
  }

  implicit val threadWrites = new Writes[Thread] {
    def writes(thread: Thread) = Json.obj(
      "member" -> thread.member.toSeq.sortBy(_.dateCreated)
    )
  }



  private val newThreadRequestForm: Form[NewThreadRequest] = Form(
    mapping(
      "authorName" -> text,
      "markdown" -> nonEmptyText
    )(NewThreadRequest.apply)(NewThreadRequest.unapply)
  )

  private val newCommentForm: Form[NewCommentRequest] = Form(
    mapping(
      "authorName" -> text,
      "markdown" -> nonEmptyText
    )(NewCommentRequest.apply)(NewCommentRequest.unapply)
  )

  def index = Action { request =>
    Ok(
        context(request) ++
        Json.obj(
          "@id" -> request.uri,
          "@type" -> "it:EntryPoint",
          "it:threads" -> routes.ThreadsApplication.create().absoluteURL(request.secure)(request)
        )
    )
  }

  def create = Action { implicit request =>
    val form = newThreadRequestForm.bindFromRequest()
    form.fold(
      hasErrors = { form =>
        BadRequest(form.toString())
      },

      success = { newThreadRequest =>
        val thread = Threads.store(
          Thread(
            newThreadRequest.authorName,
            newThreadRequest.markdown
          )
        )
        Redirect(
          routes.ThreadsApplication.get(thread.id)
        )
      }
    )
  }

  def context(request: Request[AnyContent]): JsObject = Json.obj(
      "@context" -> Json.obj(
        "hydra" -> "http://www.w3.org/ns/hydra/core#",
        "it"    -> routes.Assets.at("vocab/instathread.json#").absoluteURL(request.secure)(request)
      )
  )

  def get(id: String) = Action { request =>
    Threads.get(id).map { thread =>
      Ok(
          context(request) ++
          Json.toJson(thread).as[JsObject]
      )
    }.getOrElse(NotFound)
  }

  def postComment(id: String) = Action { implicit request =>
    Threads.get(id).map { thread =>
      val form = newCommentForm.bindFromRequest()
      form.fold(
        hasErrors = { form =>
          BadRequest(form.toString())
        },

        success = { comment =>
          Threads.store(
            Thread.post(
              thread, 
              Comment(comment.authorName, comment.markdown)
            )
          )
          Redirect(
            routes.ThreadsApplication.get(thread.id)
          )
        }
      )
    }.getOrElse(NotFound)
  }

}
