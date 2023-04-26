package deepsea.qna

import akka.actor.Actor

object QnAManager{
  case class Question(id: String, number: Int, status: String, author: String, name: String, description: String, dateCreated: Long, assignedTo: String)
}
class QnAManager extends Actor{
  override def receive: Receive = {
    case _ => None
  }
}
