package models

object Threads {
  var threads = Map[String, Thread](
    "foo" -> Thread(
      "foo",
      Set(Comment("Eric", "First!"))
    )
  )

  def store(thread: Thread): Thread = {
    threads += thread.id -> thread
    thread
  }

  def get(id: String): Option[Thread] = {
    threads.get(id)
  }

}
