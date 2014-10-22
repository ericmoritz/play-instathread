package models

case class Thread(
  id: String, 
  member: Set[Comment]
)


object Thread {
  def apply(authorName: String, markdown: String): Thread = {
    Thread(
      java.util.UUID.randomUUID.toString,
      Set(Comment(authorName, markdown))
    )
  }

  def post(thread: Thread, comment: Comment): Thread = {
    Thread(
      thread.id,
      thread.member + comment
    )
  }
}
