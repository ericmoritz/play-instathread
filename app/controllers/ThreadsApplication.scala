package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText, text}
import models.{Threads, Thread, Comment}
import play.api.libs.json._


object ThreadsApplication extends Controller {
  /************************************************************
   ** JSON writers
   ************************************************************/
  implicit var commentWrites = new Writes[Comment] {
    def writes(comment: Comment) = Json.obj(
      "vocab:authorName" -> comment.authorName,
      "schema:dateCreated" -> comment.dateCreated,
      "vocab:markdown" -> comment.markdown
    )
  }

  implicit val threadWrites = new Writes[Thread] {
    def writes(thread: Thread) = Json.obj(
      "hydra:member" -> thread.member.toSeq.sortBy(_.dateCreated)
    )
  }


  /************************************************************
   ** Forms
   ************************************************************/
  case class NewThreadRequest(authorName: String, markdown: String)
  case class NewCommentRequest(authorName: String, markdown: String)

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

  /************************************************************
   ** Actions
   ************************************************************/
  def index = Action { request =>
    Ok(
        context(request) ++
        Json.obj(
          "@id" -> request.uri,
          "@type" -> "EntryPoint",
          "vocab:threads" -> routes.ThreadsApplication.create().absoluteURL(request.secure)(request)
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
    "@context" -> routes.Assets.at("contexts/instathread.json").absoluteURL(request.secure)(request),
    "entrypoint" -> routes.ThreadsApplication.index().absoluteURL(request.secure)(request)
  )

  def get(id: String) = Action { request =>
    Threads.get(id).map { thread =>
      Ok(
          context(request) ++
          Json.toJson(thread).as[JsObject] ++
          Json.obj("@id" -> routes.ThreadsApplication.get(thread.id).absoluteURL(request.secure)(request))
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
