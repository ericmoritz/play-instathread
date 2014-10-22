package models

import com.github.nscala_time.time.Imports._

case class Comment(dateCreated: String, authorName: String, markdown: String)

object Comment {
  def apply(authorName: String, markdown: String): Comment = {
    Comment(
      DateTime.now.toString,
      authorName,
      markdown
    )
  }
}
