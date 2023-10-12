package deepsea.time

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, StatusCodes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.util.ByteString
import com.jcraft.jsch.{ChannelExec, JSch}
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.database.DBManager
import deepsea.time.PlanHoursManager.ConsumeTodayPlanHours
import deepsea.time.TimeAndWeatherManager._
import play.api.libs.json.{JsArray, Json, OWrites}

import java.io.File
import java.nio.file.Files
import java.util.{Date, Properties}
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success}


object TimeAndWeatherManager{
  case class GetTimeAndWeather()
  case class SetTimeAndWeather()
  case class CheckMaster()
  case class CheckTempFiles()

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
  private val deepSeaHost = "192.168.1.28"
  private val deepSeaPassword = "Whatab0utus"

  override def preStart(): Unit = {
    system.scheduler.scheduleWithFixedDelay(0.seconds, 5.minutes, self, SetTimeAndWeather())
    system.scheduler.scheduleWithFixedDelay(0.seconds, 1.minutes, ActorManager.planHours, ConsumeTodayPlanHours())
    system.scheduler.scheduleWithFixedDelay(0.seconds, 5.minutes, self, CheckMaster())
    system.scheduler.scheduleWithFixedDelay(0.seconds, 60.minutes, self, CheckTempFiles())
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
    case CheckMaster() =>
      if (!checkMaster){
        startMaster()
      }
    case CheckTempFiles() =>
      checkTempFiles()
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

  private def checkMaster: Boolean = {
    checkHttp("https://deep-sea.ru/rest-master/time")
  }
  private def checkHttp(url: String): Boolean = {
    val request = Http().singleRequest(HttpRequest(uri = url))
    try {
      Await.result(request, 5.seconds) match {
        case value => (value.status == StatusCodes.OK)
        case _ => false
      }
    }
    catch {
      case e: Exception =>
        println(e.toString)
        false
    }
  }
  private def startMaster(): Unit = {
    val jsch = new JSch()
    val s = jsch.getSession("root", deepSeaHost)
    val c = new Properties()
    c.put("StrictHostKeyChecking", "no")
    s.setConfig(c)
    s.setPassword(deepSeaPassword)
    s.connect()
    val ch = s.openChannel("exec").asInstanceOf[ChannelExec]
    val processName = "deepsea-master"
    val commands =
      s"tmux kill-ses -t $processName" +
        s"; tmux new -d -s $processName" +
        s"; tmux send-keys -t $processName 'cd /home/maven/deepsea-master/' Enter" +
        s"; tmux send-keys -t $processName 'sbt run' Enter"
    ch.setCommand(commands)
    ch.connect()
    ch.start()
    ch.disconnect()
    s.disconnect()
  }
  private def checkTempFiles(): Unit = {
    DBManager.GetPGConnection() match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val date = new Date().getTime
        val interval = 1000 * 60 * 60 * 24 * 3
        val limit = date - interval
        val query = s"select url from files_temp where $date < $limit"
        val rs = stmt.executeQuery(query)
        val urls = ListBuffer.empty[String]
        while (rs.next()){
          urls += rs.getString("url")
        }
        rs.close()
        stmt.execute(s"delete from files_temp where $date < $limit")
        stmt.close()
        connection.close()
        urls.foreach(url => {
          val file = url.replace(App.HTTPServer.RestUrl + "/files/", "/cloud/")
          new File(file).delete()
        })
      case _ => None
    }
  }
}
