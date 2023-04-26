package deepsea.time

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest}
import akka.http.scaladsl.model.headers.RawHeader
import akka.util.ByteString
import deepsea.database.DBManager
import deepsea.time.TimeAndWeatherManager.{GetTimeAndWeather, SetTimeAndWeather, Weather, writeWeather}
import play.api.libs.json.{JsArray, Json, OWrites}

import java.time.Duration
import java.util.Date
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}


object TimeAndWeatherManager{
  case class GetTimeAndWeather()
  case class SetTimeAndWeather()

  case class Weather(var time: Long, description: String, icon: String, temp: Double, feels_like: Double, wind: Double)

  implicit val writeWeather: OWrites[Weather] = Json.writes[Weather]
}
class TimeAndWeatherManager extends Actor{
  implicit val system: ActorSystem = ActorSystem()
  val http: HttpExt = Http(system)
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val executor: ExecutionContextExecutor = system.dispatcher
  var refresh: Long = 0
  var weather: Weather = _

  override def preStart(): Unit = {
    system.scheduler.scheduleWithFixedDelay(0.seconds, 5.minutes, self, SetTimeAndWeather())
  }
  override def receive: Receive = {
    case GetTimeAndWeather() =>
      weather.time = new Date().getTime
      sender() ! Json.toJson(weather)
    case SetTimeAndWeather() =>
      setTimeAndWeather()
//      DBManager.GetNextCloudConnection() match {
//        case Some(connection) =>
//          val stmt = connection.createStatement()
//          val query = "select * from oc_activity where file like '%0101_revB.pdf%'"
//          val rs = stmt.executeQuery(query)
//          while (rs.next()){
//            val user = rs.getString("user")
//            println(new Date().toString + " " + user)
//          }
//        case _ => None
//      }
    case _ => None
  }
  def setTimeAndWeather(): Unit ={
    val response = http.singleRequest(HttpRequest(method = HttpMethods.GET, uri = s"https://api.openweathermap.org/data/2.5/weather?id=498817&appid=2657033de497c84cb67b9fb0a39a90b4&units=metric&lang=ru&"))
    response.onComplete({
      case Success(value) =>
        value.entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
          val result = body.utf8String
          val json = Json.parse(result)

          val description = (json \ "weather").asOpt[JsArray] match {
            case Some(array) =>
              if (array.value.nonEmpty){
                (array.value.head \ "description").asOpt[String].getOrElse("")
              }
              else{
                ""
              }
            case _ => ""
          }
          val icon = (json \ "weather").asOpt[JsArray] match {
            case Some(array) =>
              if (array.value.nonEmpty){
                (array.value.head \ "icon").asOpt[String].getOrElse("")
              }
              else{
                ""
              }
            case _ => ""
          }
          val temp: Double = (json \ "main" \ "temp").asOpt[Double].getOrElse(0)
          val feels_like: Double = (json \ "main" \ "feels_like").asOpt[Double].getOrElse(0)
          val wind: Double = (json \ "wind" \ "speed").asOpt[Double].getOrElse(0)
          val time = new Date().getTime
          weather = Weather(time, description, icon, Math.round(temp), Math.round(feels_like), wind)
        }
      case Failure(exception) =>
        println(exception)
    })
  }
}
