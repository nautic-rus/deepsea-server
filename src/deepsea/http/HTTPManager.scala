package deepsea.http

import akka.Done
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.{Actor, ActorRef}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Multipart.BodyPart
import akka.http.scaladsl.model.{HttpEntity, Multipart}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import akka.pattern.ask
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.Timeout
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.HTTPManagerStarted
import deepsea.auth.AuthManager.{GetUsers, Login, ShareRights}
import deepsea.fest.FestManager.{DeleteFestKaraoke, DeleteFestSauna, DeleteFestStories, GetBestPlayers, GetFestKaraoke, GetFestSauna, GetFestStories, GetMarks, GetTeamsWon, SetBestPlayer, SetFestKaraoke, SetFestSauna, SetFestStories, SetMarks, SetTeamsWon}
import deepsea.files.FileManager.{CreateFile, GetPdSpList}
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManager._
import deepsea.materials.MaterialManager.{GetMaterialNodes, GetMaterials, GetWCDrawings, GetWCZones, GetWeightControl, SetWeightControl, UpdateMaterial, UpdateMaterialNode}
import deepsea.mobile.MobileManager.{GetDrawingInfo, GetDrawings}
import deepsea.rocket.RocketChatManager.SendNotification
import deepsea.time.LicenseManager.GetForanLicenses
import deepsea.time.TimeAndWeatherManager.GetTimeAndWeather
import deepsea.time.TimeControlManager.{AddUserWatch, GetUserTimeControl, GetUserWatches}
import org.apache.log4j.{LogManager, Logger}
import play.api.libs.json.{JsValue, Json}

import java.io.{File, FileInputStream, InputStream}
import java.util.{Date, UUID}
import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object HTTPManager{
  case class Response(value: String)
}
class HTTPManager extends Actor{
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "http")
  implicit val executionContext: ExecutionContextExecutor = system.executionContext
  implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)
  val logger: Logger = LogManager.getLogger("HttpManager")
  var server:  Future[Http.ServerBinding] = _
  val routes: Route = cors() {
    concat(
      //AUTHORIZATION COMMANDS
      (get & path("login") & parameter("login", "password")){(login, password) =>
        askFor(ActorManager.auth, Login(Option.empty[String], login, password))
      },
      (get & path("login") & parameter("token")){ token =>
        askFor(ActorManager.auth, Login(Option(token)))
      },
      (get & path("users")){
        askFor(ActorManager.auth, GetUsers())
      },
      //ISSUE MANAGER COMMANDS
      (get & path("issueProjects")){
        askFor(ActorManager.issue, GetIssueProjects())
      },
      (get & path("issueTypes")){
        askFor(ActorManager.issue, GetIssueTypes())
      },
      (get & path("issuePriorities")){
        askFor(ActorManager.issue, GetIssuePriorities())
      },
      (get & path("issueDepartments")){
        askFor(ActorManager.issue, GetIssueDepartments())
      },
      (get & path("issues") & parameter("user")){ user =>
        askFor(ActorManager.issue, GetIssues(user))
      },
      (post & path("startIssue") & entity(as[String])){ (issue) =>
        askFor(ActorManager.issue, StartIssue(issue))
      },
      (post & path("updateIssue") & parameter("user") & parameter("message") & entity(as[String])){ (user, message, issue) =>
        askFor(ActorManager.issue, UpdateIssue(user, message, issue))
      },
      (get & path("assignIssue") & parameter("id") & parameter("user") & parameter("startDate")  & parameter("dueDate") & parameter("overtime") & parameter("action") & parameter("author")){ (id, user, startDate, dueDate, overtime, action, author) =>
        askFor(ActorManager.issue, AssignIssue(id, user, startDate, dueDate, overtime, action, author))
      },
      (get & path("changeResponsible") & parameter("id") & parameter("user") & parameter("author") & parameter("action")){ (id, user, author, action) =>
        askFor(ActorManager.issue, ChangeResponsible(id, user, author, action))
      },
      (get & path("removeIssue") & parameter("id") & parameter("user")){ (id, user) =>
        askFor(ActorManager.issue, RemoveIssue(id, user))
      },
      (get & path("issueDetails") & parameter("id")){ (id) =>
        askFor(ActorManager.issue, GetIssueDetails(id))
      },
      (get & path("setIssueViewed") & parameter("id") & parameter("user")){ (id, user) =>
        askFor(ActorManager.issue, SetIssueViewed(id, user))
      },
      (get & path("issuesViewed") & parameter("user")){ (user) =>
        askFor(ActorManager.issue, GetIssuesViewed(user))
      },
      (post & path("setIssueMessage") & entity(as[String]) & parameter("id")){ (message, id) =>
        askFor(ActorManager.issue, SetIssueMessage(id, message))
      },
      (get & path("setDayCalendar") & parameter("user") & parameter("day") & parameter("status")){ (user, day, status) =>
        askFor(ActorManager.issue, SetDayCalendar(user, day, status))
      },
      (get & path("daysCalendar")){
        askFor(ActorManager.issue, GetCalendar())
      },
      (get & path("deleteFile") & parameter("url")){ (url) =>
        askFor(ActorManager.issue, DeleteFile(url))
      },
      (get & path("issuePeriods")){
        askFor(ActorManager.issue, GetIssuePeriods())
      },
      (post & path("setRevisionFiles") & entity(as[String]) & parameter("id") & parameter("revision")){ (files, id, revision) =>
        askFor(ActorManager.issue, SetRevisionFiles(id, revision, files))
      },
      (get & path("deleteRevisionFile") & parameter("file_url") & parameter("user")){ (file_url, user) =>
        askFor(ActorManager.issue, DeleteRevisionFile(file_url, user))
      },
      (get & path("clearRevisionFiles") & parameter("issueId") & parameter("user") & parameter("fileGroup") & parameter("revision")){ (issueId, user, fileGroup, revision) =>
        askFor(ActorManager.issue, ClearRevisionFiles(issueId, user, fileGroup, revision))
      },
      (get & path("revisionFiles")){
        askFor(ActorManager.issue, GetRevisionFiles())
      },
      (get & path("activeLicenses")){
        askFor(ActorManager.license, GetForanLicenses())
      },
      (get & path("timeControl") & parameter("user")){ (user) =>
        askFor(ActorManager.timeControl, GetUserTimeControl(user))
      },
      (get & path("setLabor") & parameter("user") & parameter("issue_id") & parameter("labor_value") & parameter("labor_comment") & parameter("date")){ (user, issue_id, labor_value, labor_comment, date) =>
        askFor(ActorManager.issue, SetIssueLabor(user, issue_id, labor_value, labor_comment, date))
      },
      (get & path("shareRights") & parameter("user") & parameter("with_user")){ (user, with_user) =>
        askFor(ActorManager.auth, ShareRights(user, with_user))
      },
      (get & path("sfiCodes")){
        askFor(ActorManager.issue, GetSfiCodes())
      },
      (get & path("timeAndWeather")){
        askFor(ActorManager.timeAndWeather, GetTimeAndWeather())
      },
      (get & path("issueSpentTime")){
        askFor(ActorManager.issue, GetIssueSpentTime())
      },
      (post & path("setIssueChecks") & entity(as[String]) & parameter("issue_id")){ (checks, issue_id) =>
        askFor(ActorManager.issue, SetIssueChecks(issue_id, checks))
      },
      (get & path("updateIssueCheck") & parameter("issue_id") & parameter("user") & parameter("check_description") & parameter("check_group") & parameter("check_status")){ (issue_id, user, check_description, check_group, check_status) =>
        askFor(ActorManager.issue, UpdateIssueCheck(issue_id, user, check_description, check_group, check_status))
      },
      (get & path("checkTemplates") & parameter("user")){ (user) =>
        askFor(ActorManager.issue, GetCheckTemplates(user))
      },
      (get & path("nestingFiles")){
        askFor(ActorManager.issue, GetNestingFiles())
      },
      (get & path("getAmountTask") & parameter("project") & parameter("status") & parameter("department")){ (project,status,department) =>
        askFor(ActorManager.issue, GetAmountTask(project,status,department))
      },
      (get & path("weightControl")){
        askFor(ActorManager.materials, GetWeightControl())
      },
      (post & path("setWeightControl") & entity(as[String])){ (controlValue) =>
        askFor(ActorManager.materials, SetWeightControl(controlValue))
      },
      (get & path("wcDrawings")){
        askFor(ActorManager.materials, GetWCDrawings())
      },
      (get & path("wcZones")){
        askFor(ActorManager.materials, GetWCZones())
      },
      (get & path("mobileDrawings")){
        askFor(ActorManager.mobile, GetDrawings())
      },
      (get & path("mobileDrawingInfo") & parameter("drawing")){ (drawing) =>
        askFor(ActorManager.mobile, GetDrawingInfo(drawing))
      },
      //FILE MANAGER COMMANDS
//      (post & path("createFileUrl") & entity(as[Multipart.FormData])){ formData =>
//        var fileName = ""
//        var fileStream: InputStream = null
//        val done: Future[Done] = formData.parts.mapAsync(1) {
//          case b: BodyPart if b.name == "file" =>
//            val file = File.createTempFile("upload", "tmp")
//            b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
//            fileName = b.filename.get
//            fileStream = new FileInputStream(file)
//            file.delete()
//            Future.successful(Done)
//          case _ => Future.successful(Done)
//        }.runWith(Sink.ignore)
//        onSuccess(done) { _ =>
//          askFor(ActorManager.files, CreateFile(fileName, fileStream), long = true)
//        }
//      },
      (post & path("createFileUrl") & entity(as[Multipart.FormData]) & parameter("user") ){ (formData, user) =>
        var fileName = ""
        var fileUrl = ""
        val date = new Date().getTime
        val done: Future[Done] = formData.parts.mapAsync(1) {
          case b: BodyPart if b.name == "file" =>
            fileName = b.filename.get
            var pathId = UUID.randomUUID().toString.substring(0, 8)
            var file = new File(App.Cloud.Directory + "/" + pathId)
            while (file.exists()){
              pathId = UUID.randomUUID().toString.substring(0, 8)
              file = new File(App.Cloud.Directory + "/" + pathId)
            }
            file.mkdir()
            file = new File(App.Cloud.Directory + "/" + pathId + "/" + fileName)
            fileUrl = App.Cloud.Url + "/" + pathId + "/" + fileName
            b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
            Future.successful(Done)
          case _ => Future.successful(Done)
        }.runWith(Sink.ignore)
        onSuccess(done) { _ =>
          complete(HttpEntity(Json.toJson(new FileAttachment(fileName, fileUrl, date, user)).toString()))
        }
      },
      (post & path("createFileCloudUrl") & entity(as[Multipart.FormData]) & parameter("filePath") & parameter("login") & parameter("password")){ (formData, filePath, login, password) =>
        var fileName = ""
        var fileStream: InputStream = null
        val done: Future[Done] = formData.parts.mapAsync(1) {
          case b: BodyPart if b.name == "file" =>
            val file = File.createTempFile("upload", "tmp")
            b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
            fileName = b.filename.get
            fileStream = new FileInputStream(file)
            file.delete()
            Future.successful(Done)
          case _ => Future.successful(Done)
        }.runWith(Sink.ignore)
        onSuccess(done) { _ =>
          askFor(ActorManager.files, CreateFile(fileName, fileStream, filePath, login, password), long = true)
        }
      },
      (get & path("files" / Segment / Segment)){ (path, name) =>
        getFromFile(App.Cloud.Directory + "/" + path + "/" + name)
      },
      //ISSUE MANAGER COMMANDS
      (get & path("getPdSpList")){
        askFor(ActorManager.files, GetPdSpList())
      },
      //MATERIAL COMMANDS
      (get & path("materials") & parameter("project")){ project =>
        askFor(ActorManager.materials, GetMaterials(project))
      },
      (get & path("updateMaterial") & parameter("material") & parameter("user") & parameter("remove")){ (material, user, remove) =>
        askFor(ActorManager.materials, UpdateMaterial(material, user, remove))
      },
      (get & path("materialNodes")){
        askFor(ActorManager.materials, GetMaterialNodes())
      },
      (get & path("updateMaterialNode") & parameter("data") & parameter("label") & parameter("user") & parameter("remove")){ (data, label, user, remove) =>
        askFor(ActorManager.materials, UpdateMaterialNode(data, label, user, remove))
      },

      //FEST
      (post & path("createFestFileUrl") & entity(as[Multipart.FormData]) ){ (formData) =>
        var fileName = ""
        var fileUrl = ""
        val date = new Date().getTime
        val done: Future[Done] = formData.parts.mapAsync(1) {
          case b: BodyPart if b.name == "file" =>
            fileName = b.filename.get
            var pathId = UUID.randomUUID().toString.substring(0, 8)
            var file = new File(App.Cloud.Directory + "/" + pathId)
            while (file.exists()){
              pathId = UUID.randomUUID().toString.substring(0, 8)
              file = new File(App.Cloud.Directory + "/" + pathId)
            }
            file.mkdir()
            file = new File(App.Cloud.Directory + "/" + pathId + "/" + fileName)
            fileUrl = App.Cloud.FestUrl + "/" + pathId + "/" + fileName
            b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
            Future.successful(Done)
          case _ => Future.successful(Done)
        }.runWith(Sink.ignore)
        onSuccess(done) { _ =>
          complete(HttpEntity(Json.toJson(new FileAttachment(fileName, fileUrl, date, "fest")).toString()))
        }
      },
      (get & path("festStories")){
        askFor(ActorManager.fest, GetFestStories())
      },
      (get & path("setFestStories") & parameter("url") & parameter("thumb")){ (url, thumb) =>
        askFor(ActorManager.fest, SetFestStories(url, thumb))
      },
      (get & path("deleteFestStories") & parameter("url")){ (url) =>
        askFor(ActorManager.fest, DeleteFestStories(url))
      },

      (get & path("festKaraoke")){
        askFor(ActorManager.fest, GetFestKaraoke())
      },
      (get & path("setFestKaraoke") & parameter("users") & parameter("song")){ (users, song) =>
        askFor(ActorManager.fest, SetFestKaraoke(users, song))
      },
      (get & path("deleteFestKaraoke") & parameter("time")){ (time) =>
        askFor(ActorManager.fest, DeleteFestKaraoke(time))
      },

      (get & path("festSauna")){
        askFor(ActorManager.fest, GetFestSauna())
      },
      (get & path("setFestSauna") & parameter("kind") & parameter("users") & parameter("time")){ (kind, users, time) =>
        askFor(ActorManager.fest, SetFestSauna(kind, users, time))
      },
      (get & path("deleteFestSauna") & parameter("time")){ (time) =>
        askFor(ActorManager.fest, DeleteFestSauna(time))
      },

      (get & path("teamsWon")){
        askFor(ActorManager.fest, GetTeamsWon())
      },
      (post & path("setTeamsWon") & entity(as[String])){ (teamsWon) =>
        askFor(ActorManager.fest, SetTeamsWon(teamsWon))
      },

      (get & path("marks")){
        askFor(ActorManager.fest, GetMarks())
      },
      (post & path("setMarks") & entity(as[String])){ (marks) =>
        askFor(ActorManager.fest, SetMarks(marks))
      },

      (get & path("marks")){
        askFor(ActorManager.fest, GetMarks())
      },
      (post & path("setMarks") & entity(as[String])){ (marks) =>
        askFor(ActorManager.fest, SetMarks(marks))
      },

      (get & path("bestPlayers")){
        askFor(ActorManager.fest, GetBestPlayers())
      },
      (post & path("setBestPlayer") & entity(as[String])){ (bestPlayer) =>
        askFor(ActorManager.fest, SetBestPlayer(bestPlayer))
      },
      (get & path("sendNotificationToUser") & parameter("user") & parameter("message")){ (user, message) =>
        askFor(ActorManager.rocket, SendNotification(user, message))
      },

      (get & path("messageReactions")){
        askFor(ActorManager.issue, GetMessageReactions())
      },
      (post & path("setMessageReaction") & entity(as[String])){ (reaction) =>
        askFor(ActorManager.issue, SetMessageReaction(reaction))
      },
      (get & path("deleteMessageReaction") & parameter("id")){ (id) =>
        askFor(ActorManager.issue, DeleteMessageReaction(id.toIntOption.getOrElse(0)))
      },
      (get & path("ping")){
        complete(HttpEntity("pong"))
      },

      (post & path("grabInfo") & entity(as[String])){ (data) =>
        askFor(ActorManager.timeControl, AddUserWatch(data))
      },
      (get & path("userWatches")){
        askFor(ActorManager.timeControl, GetUserWatches())
      },

    )
  }

  def askFor(actor: ActorRef, command: Any, long: Boolean = false): StandardRoute ={
    try{
      Await.result(actor ? command, timeout.duration) match {
        case response: JsValue => complete(HttpEntity(response.toString()))
        case response: Array[Byte] => complete(HttpEntity(response))
        case response: String => complete(HttpEntity(response))
        case response: io.circe.Json => complete(HttpEntity(response.noSpaces))
        case response: HttpEntity.Strict => complete(response)
        case _ => complete(HttpEntity(Json.toJson("Error: Wrong response from actor.").toString()))
      }
    }
    catch {
      case _: Throwable => complete(HttpEntity(Json.toJson("Error: No response from actor in timeout.").toString()))
    }
  }
  def error: StandardRoute = complete(HttpEntity(Json.toJson("Error: Wrong response.").toString()))
  override def preStart(): Unit = {
    server = Http().newServerAt(App.HTTPServer.Host, App.HTTPServer.Port).bind(routes)
    logger.debug("HTTP server has been started at " + App.HTTPServer.Host + " with port " + App.HTTPServer.Port)
    ActorManager.startup ! HTTPManagerStarted()
  }
  override def postStop(): Unit = {
    server.flatMap(_.unbind()).onComplete(_ => system.terminate())
  }
  override def receive: Receive = {
    case _ => None
  }
}
