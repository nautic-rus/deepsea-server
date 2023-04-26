package deepsea.rocket

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest}
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.ByteString
import deepsea.rocket.RocketChatManager.SendNotification
import play.api.libs.json.Json

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object RocketChatManager{
  case class SendMessage(toUser: String, toMail: String, subject: String, message: String)
  case class SendNotification(toUser: String, message: String)
}
class RocketChatManager extends Actor{
  implicit val system: ActorSystem = ActorSystem()
  val http: HttpExt = Http(system)
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val executor: ExecutionContextExecutor = system.dispatcher

  override def preStart(): Unit = {

  }
  override def receive: Receive = {
    case msg: SendNotification =>
      val response = http.singleRequest(HttpRequest(method = HttpMethods.POST, uri = s"https://rocket.nautic-rus.com/api/v1/login", entity = HttpEntity(ContentTypes.`application/json`, "{\"user\":\"deepsea\",\"password\":\"Ship1234\"}")))
      response.onComplete({
        case Success(value) =>
          value.entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
            val result = body.utf8String
            val json = Json.parse(result)
            val userId = (json \ "data" \ "userId").asOpt[String] match {
              case Some(userId) => userId
              case _ => ""
            }
            val authToken = (json \ "data" \ "authToken").asOpt[String] match {
              case Some(userId) => userId
              case _ => ""
            }
            val h1 = new RawHeader("X-User-Id", userId)
            val h2 = new RawHeader("X-Auth-Token", authToken)
            val postMessage = http.singleRequest(HttpRequest(method = HttpMethods.POST, uri = s"https://rocket.nautic-rus.com/api/v1/chat.postMessage", headers = List(h1, h2), entity = HttpEntity(ContentTypes.`application/json`, "{\"channel\":\"@" + msg.toUser + "\",\"text\":\"" + msg.message + "\"}")))
          }
        case Failure(exception) =>
          println(exception)
      })
      sender() ! Json.toJson("success")
    case _ => None
  }
}
