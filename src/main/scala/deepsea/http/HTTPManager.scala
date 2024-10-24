package deepsea.http

import akka.Done
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.{Actor, ActorRef}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity.{ChunkStreamPart, Chunked}
import akka.http.scaladsl.model.Multipart.BodyPart
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.ContentTypeResolver
import akka.http.scaladsl.server.{ExceptionHandler, Route, StandardRoute}
import akka.pattern.ask
import akka.stream.scaladsl.{FileIO, Sink}
import akka.util.Timeout
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.HTTPManagerStarted
import deepsea.auth.AuthManager.{CreateUsersNotifications, DeleteAdminRight, DeleteRole, DeleteUser, EditAdminRight, EditRole, EditUser, EditUsersProject, GetAdminRightDetails, GetAdminRights, GetDepartmentDetails, GetDepartments, GetPages, GetRightDetails, GetRights, GetRoleDetails, GetRoleRights, GetRoles, GetUserDetails, GetUserVisibleProjects, GetUsers, GetUsersNotifications, GetUsersProject, JoinUsersProjects, Login, SaveRoleForAll, SendLogPass, ShareRights, StartRight, StartRole, StartUser, UpdateEmail, UpdateRocketLogin, UpdateUsersNotifications}
import deepsea.dbase.DBManager
import deepsea.fest.FestManager.{DeleteFestKaraoke, DeleteFestSauna, DeleteFestStories, GetBestPlayers, GetFestKaraoke, GetFestSauna, GetFestStories, GetMarks, GetTeamsWon, SetBestPlayer, SetFestKaraoke, SetFestSauna, SetFestStories, SetMarks, SetTeamsWon}
import deepsea.files.FileManager.{CreateDocumentCloudDirectory, CreateFile, CreateMaterialCloudDirectory, GetCloudFiles, GetDocumentFiles, GetFileFromCloud, GetFileFromMongo, GetPdSpList, MongoFile, UploadFileToMongo}
import deepsea.files.classes.FileAttachment
import deepsea.http.HTTPManager.server
import deepsea.issues.IssueManager._
import deepsea.materials.MaterialManager.{AddEquipFile, AddMaterialCheck, AddMaterialComplect, AddSupFile, AddSupMatRelation, AddSupName, AddSupplierHistory, DelEquipFile, DelRelatedTask, DelSupFile, DeleteEquipment, DeleteMaterialCheck, DeleteSupplier, GetEqSupMatRelations, GetEquipFiles, GetEquipment, GetEquipments, GetMaterialCheck, GetMaterialComplects, GetMaterialNodes, GetMaterials, GetMaterialsCode, GetRelatedTasks, GetSFIs, GetSpecDirectories, GetSpecMaterials, GetSpecStatements, GetSupFiles, GetSupMatRelations, GetSupNames, GetSupStatuses, GetSupplierHistory, GetWCDrawings, GetWCZones, GetWeightControl, InsertEquipment, InsertSupplier, RemoveMaterialComplect, RemoveWeightControl, SetWeightControl, SupTaskAdd, UpdateMaterial, UpdateMaterialComplect, UpdateMaterialDirectory, UpdateMaterialNode, UpdateSpecMaterial}
import deepsea.mobile.MobileManager.{GetDrawingInfo, GetDrawings}
import deepsea.osm.OsmManager.{AddPLS, GetPLS}
import deepsea.rocket.RocketChatManager.SendNotification
import deepsea.storage.StorageManager.{GetNewStorageUnit, GetStorageFiles, GetStorageLocations, GetStorageUnits, UpdateStorageFile, UpdateStorageLocation, UpdateStorageUnit}
import deepsea.time.LicenseManager.GetForanLicenses
import deepsea.time.PlanHoursManager.{ConsumePlanHours, DeleteUserTask, GetConsumedHours, GetPlannedHours, GetUserPlanHours, PlanUserTask, SavePlannedHours}
import deepsea.time.TimeAndWeatherManager.GetTimeAndWeather
import deepsea.time.TimeControlManager.{AddSpyWatch, AddUserWatch, GetSpyWatches, GetTime, GetUserTimeControl, GetUserWatches}
import deepsea.time.PlanManager._
import io.circe.syntax.EncoderOps
import org.apache.log4j.{LogManager, Logger}
import play.api.libs.json.{JsValue, Json}

import java.io.{File, FileInputStream, InputStream}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.util.{Calendar, Date, UUID}
import java.util.concurrent.TimeUnit
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.io.Source
import scala.util.{Failure, Success}

object HTTPManager {
  case class Response(value: String)

  var server: Future[Http.ServerBinding] = _

  def check(): String = {
    server.value.get.get.localAddress.toString
  }
}

class HTTPManager extends Actor {
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "http")
  implicit val executionContext: ExecutionContextExecutor = system.executionContext
  implicit val timeout: Timeout = Timeout(350, TimeUnit.SECONDS)
  val logger: Logger = LogManager.getLogger("HttpManager")
  val routes: Route = cors() {
    extractClientIP { ip =>
      //logIp(ip)
      concat(
        //AUTHORIZATION COMMANDS
        (get & path("login") & parameter("login", "password")) { (login, password) =>
          askFor(ActorManager.auth, Login(Option.empty[String], login, password))
        },
        (get & path("login") & parameter("token")) { token =>
          askFor(ActorManager.auth, Login(Option(token)))
        },
        (get & path("users")) {
          askFor(ActorManager.auth, GetUsers())
        },
        (get & path("rights")) {
          askFor(ActorManager.auth, GetRights())
        },
        (get & path("rightDetails") & parameter("id")) { id =>
          askFor(ActorManager.auth, GetRightDetails(id))
        },
        (get & path("userDetails") & parameter("id")) { id =>
          askFor(ActorManager.auth, GetUserDetails(id))
        },
        (get & path("userNotifications") & parameter("id")) { id =>
          askFor(ActorManager.auth, GetUsersNotifications(id))
        },
        (post & path("updateNotification") & entity(as[String]) & parameter("userId")) { (notifications, userId) =>
          askFor(ActorManager.auth, UpdateUsersNotifications(notifications, userId))
        },
        (post & path("createNotification") & entity(as[String])) { notification =>
          askFor(ActorManager.auth, CreateUsersNotifications(notification))
        },
        (post & path("startUser") & entity(as[String])) { (user) =>
          askFor(ActorManager.auth, StartUser(user))
        },
        (get & path("deleteUser") & parameter("id")) { id =>
          askFor(ActorManager.auth, DeleteUser(id))
        },
        (post & path("editUser") & entity(as[String]) & parameter("id")) { (role, id) =>
          askFor(ActorManager.auth, EditUser(role, id))
        },
        (post & path("editUsersProject") & entity(as[String]) & parameter("idProject")) { (idUsers, idProject) =>
          askFor(ActorManager.auth, EditUsersProject(idUsers, idProject))
        },
        //        (get & path("joinUsersProjects")) {
        //          askFor(ActorManager.auth, JoinUsersProjects())
        //        },
        (get & path("usersProject") & parameter("id")) { id =>
          askFor(ActorManager.auth, GetUsersProject(id))
        },
        (get & path("userVisibleProjects") & parameter("id")) { id =>
          askFor(ActorManager.auth, GetUserVisibleProjects(id))
        },
        (get & path("pages")) {
          askFor(ActorManager.auth, GetPages())
        },
        (get & path("sendLogPass") & parameter("id")) { id =>
          askFor(ActorManager.auth, SendLogPass(id))
        },
        //ROLES
        (get & path("adminRoles")) {
          askFor(ActorManager.auth, GetRoles())
        },
        (get & path("roleDetails") & parameter("name")) { name =>
          askFor(ActorManager.auth, GetRoleDetails(name))
        },
        (get & path("roleRights") & parameter("name")) { name =>
          askFor(ActorManager.auth, GetRoleRights(name))
        },
        (post & path("startRole") & entity(as[String])) { (role) =>
          askFor(ActorManager.auth, StartRole(role))
        },
        (post & path("setRoleForAll") & parameter("name")) { name =>
          askFor(ActorManager.auth, SaveRoleForAll(name))
        },
        (get & path("deleteRole") & parameter("name")) { name =>
          askFor(ActorManager.auth, DeleteRole(name))
        },
        (post & path("editRole") & entity(as[String]) & parameter("name")) { (role, name) =>
          askFor(ActorManager.auth, EditRole(role, name))
        },
        (get & path("adminRights")) {
          askFor(ActorManager.auth, GetAdminRights())
        },
        (post & path("startRight") & entity(as[String])) { (right) =>
          askFor(ActorManager.auth, StartRight(right))
        },
        (get & path("adminRightDetails") & parameter("name")) { name =>
          askFor(ActorManager.auth, GetAdminRightDetails(name))
        },
        (get & path("deleteAdminRight") & parameter("name")) { name =>
          askFor(ActorManager.auth, DeleteAdminRight(name))
        },
        (post & path("editAdminRight") & entity(as[String]) & parameter("name")) { (right, name) =>
          askFor(ActorManager.auth, EditAdminRight(right, name))
        },
        (get & path("departments")) {
          askFor(ActorManager.auth, GetDepartments())
        },
        (get & path("departmentDetails") & parameter("id")) { id =>
          askFor(ActorManager.auth, GetDepartmentDetails(id))
        },
        //PROJECTS
        (get & path("projectDetails") & parameter("id")) { id =>
          askFor(ActorManager.issue, GetProjectDetails(id))
        },
        (get & path("projectContracts") & parameter("project")) { project =>
          askFor(ActorManager.issue, GetProjectContracts(project))
        },
        (post & path("startProject") & entity(as[String])) { (project) =>
          askFor(ActorManager.issue, StartProject(project))
        },
        (get & path("deleteProject") & parameter("id")) { id =>
          askFor(ActorManager.issue, DeleteProject(id))
        },
        (post & path("editProject") & entity(as[String]) & parameter("id")) { (project, id) =>
          askFor(ActorManager.issue, EditProject(project, id))
        },
        //ISSUE MANAGER COMMANDS
        (get & path("issueProjects")) {
          askFor(ActorManager.issue, GetIssueProjects())
        },
        (get & path("issueTypes")) {
          askFor(ActorManager.issue, GetIssueTypes())
        },
        (get & path("issuePriorities")) {
          askFor(ActorManager.issue, GetIssuePriorities())
        },
        (get & path("issueDepartments")) {
          askFor(ActorManager.issue, GetIssueDepartments())
        },
        (get & path("issues") & parameter("user")) { user =>
          askFor(ActorManager.issue, GetIssues(user))
        },
        (get & path("issuesAll")) {
          askFor(ActorManager.issue, GetAllIssues())
        },
        (get & path("issuesAllShort")) {
          askFor(ActorManager.issue, GetAllIssuesShort())
        },
        (get & path("issues-correction")) {
          askFor(ActorManager.issue, GetIssuesCorrection())
        },
        (get & path("questions")) {
          askFor(ActorManager.issue, GetQuestions())
        },
        (post & path("startIssue") & entity(as[String])) { (issue) =>
          askFor(ActorManager.issue, StartIssue(issue))
        },
        (post & path("addFilesInIssue") & entity(as[String])) { (file) =>
          askFor(ActorManager.issue, AddFilesInIssue(file))
        },
        (post & path("updateIssue") & parameter("user") & parameter("message") & entity(as[String])) { (user, message, issue) =>
          askFor(ActorManager.issue, UpdateIssue(user, message, issue))
        },
        (get & path("assignIssue") & parameter("id") & parameter("user") & parameter("startDate") & parameter("dueDate") & parameter("overtime") & parameter("action") & parameter("author") & parameter("hidden")) { (id, user, startDate, dueDate, overtime, action, author, hidden) =>
          askFor(ActorManager.issue, AssignIssue(id, user, startDate, dueDate, overtime, action, author, hidden))
        },
        (get & path("changeResponsible") & parameter("id") & parameter("user") & parameter("author") & parameter("action")) { (id, user, author, action) =>
          askFor(ActorManager.issue, ChangeResponsible(id, user, author, action))
        },
        (get & path("removeIssue") & parameter("id") & parameter("user")) { (id, user) =>
          askFor(ActorManager.issue, RemoveIssue(id, user))
        },
        (get & path("combineIssues") & parameter("firstIssue") & parameter("secondIssue") & parameter("user")) { (firstIssue, secondIssue, user) =>
          askFor(ActorManager.issue, CombineIssues(firstIssue, secondIssue, user))
        },
        (get & path("unCombineIssues") & parameter("firstIssue") & parameter("secondIssue") & parameter("user")) { (firstIssue, secondIssue, user) =>
          askFor(ActorManager.issue, UnCombineIssues(firstIssue, secondIssue, user))
        },
        (get & path("issueDetails") & parameter("id")) { (id) =>
          askFor(ActorManager.issue, GetIssueDetails(id))
        },
        (get & path("issueDetails") & parameter("docNumber")) { (docNumber) =>
          askFor(ActorManager.issue, GetIssueDetailsByDocNumber(docNumber))
        },
        (get & path("setIssueViewed") & parameter("id") & parameter("user")) { (id, user) =>
          askFor(ActorManager.issue, SetIssueViewed(id, user))
        },
        (get & path("issuesViewed") & parameter("user")) { (user) =>
          askFor(ActorManager.issue, GetIssuesViewed(user))
        },
        (post & path("setIssueMessage") & entity(as[String]) & parameter("id")) { (message, id) =>
          askFor(ActorManager.issue, SetIssueMessage(id, message))
        },
        (get & path("setDayCalendar") & parameter("user") & parameter("day") & parameter("status")) { (user, day, status) =>
          askFor(ActorManager.issue, SetDayCalendar(user, day, status))
        },
        (get & path("daysCalendar")) {
          askFor(ActorManager.issue, GetCalendar())
        },
        (get & path("deleteFile") & parameter("url")) { (url) =>
          askFor(ActorManager.issue, DeleteFile(url))
        },
        (get & path("issuePeriods")) {
          askFor(ActorManager.issue, GetIssuePeriods())
        },
        (get & path("setIssuePeriods") & parameter("id", "start", "end")) { (id, start, end) =>
          askFor(ActorManager.issue, SetIssuePeriods(id, start, end))
        },
        (get & path("reasonsOfChange")) {
          askFor(ActorManager.issue, GetReasonsOfChange())
        },
        (post & path("issuesFiles") & entity(as[String]) & parameter("user", "email")) { (ids, user, email) =>
          askFor(ActorManager.issue, GetIssuesFiles(ids, user, email))
        },
        (post & path("setRevisionFiles") & entity(as[String]) & parameter("id") & parameter("revision")) { (files, id, revision) =>
          askFor(ActorManager.issue, SetRevisionFiles(id, revision, files))
        },
        (get & path("deleteRevisionFile") & parameter("file_url") & parameter("user")) { (file_url, user) =>
          askFor(ActorManager.issue, DeleteRevisionFile(file_url, user))
        },
        (get & path("clearRevisionFiles") & parameter("issueId") & parameter("user") & parameter("fileGroup") & parameter("revision")) { (issueId, user, fileGroup, revision) =>
          askFor(ActorManager.issue, ClearRevisionFiles(issueId, user, fileGroup, revision))
        },
        (get & path("revisionFiles")) {
          askFor(ActorManager.issue, GetRevisionFiles())
        },
        (get & path("activeLicenses")) {
          askFor(ActorManager.license, GetForanLicenses())
        },
        (get & path("timeControl") & parameter("user")) { (user) =>
          askFor(ActorManager.timeControl, GetUserTimeControl(user))
        },
        (get & path("setLabor") & parameter("user") & parameter("issue_id") & parameter("labor_value") & parameter("labor_comment") & parameter("date")) { (user, issue_id, labor_value, labor_comment, date) =>
          askFor(ActorManager.issue, SetIssueLabor(user, issue_id, labor_value, labor_comment, date))
        },
        (get & path("shareRights") & parameter("user") & parameter("with_user")) { (user, with_user) =>
          askFor(ActorManager.auth, ShareRights(user, with_user))
        },
        (get & path("sfiCodes")) {
          askFor(ActorManager.issue, GetSfiCodes())
        },
        (get & path("timeAndWeather")) {
          askFor(ActorManager.timeAndWeather, GetTimeAndWeather())
        },
        (post & path("setIssueChecks") & entity(as[String]) & parameter("issue_id")) { (checks, issue_id) =>
          askFor(ActorManager.issue, SetIssueChecks(issue_id, checks))
        },
        (get & path("updateIssueCheck") & parameter("issue_id") & parameter("user") & parameter("check_description") & parameter("check_group") & parameter("check_status")) { (issue_id, user, check_description, check_group, check_status) =>
          askFor(ActorManager.issue, UpdateIssueCheck(issue_id, user, check_description, check_group, check_status))
        },
        (get & path("checkTemplates") & parameter("user")) { (user) =>
          askFor(ActorManager.issue, GetCheckTemplates(user))
        },
        (get & path("nestingFiles")) {
          askFor(ActorManager.issue, GetNestingFiles())
        },
        (get & path("getAmountTask") & parameter("project") & parameter("status") & parameter("department")) { (project, status, department) =>
          askFor(ActorManager.issue, GetAmountTask(project, status, department))
        },
        (get & path("weightControl")) {
          askFor(ActorManager.materials, GetWeightControl())
        },
        (post & path("setWeightControl") & entity(as[String])) { (controlValue) =>
          askFor(ActorManager.materials, SetWeightControl(controlValue))
        },
        (post & path("removeWeightControl") & entity(as[String]) & parameter("user")) { (controlValue, user) =>
          askFor(ActorManager.materials, RemoveWeightControl(controlValue, user))
        },
        (get & path("wcDrawings")) {
          askFor(ActorManager.materials, GetWCDrawings())
        },
        (get & path("wcZones")) {
          askFor(ActorManager.materials, GetWCZones())
        },
        (get & path("projectNames")) {
          askFor(ActorManager.issue, GetProjectNames())
        },
        (get & path("mobileDrawings")) {
          askFor(ActorManager.mobile, GetDrawings())
        },
        (get & path("mobileDrawingInfo") & parameter("drawing")) { (drawing) =>
          askFor(ActorManager.mobile, GetDrawingInfo(drawing))
        },


        (get & path("userPlanHours") & parameter("userId", "startDate", "available")) { (userId, startDate, available) =>
          askFor(ActorManager.planHours, GetUserPlanHours(userId, startDate, available))
        },
        (get & path("planUserTask") & parameter("userId", "taskId", "fromHour", "amountOfHours", "allowMove")) { (userId, taskId, fromHour, amountOfHours, allowMove) =>
          askFor(ActorManager.planHours, PlanUserTask(userId, taskId, fromHour, amountOfHours, allowMove))
        },
        (get & path("deleteUserTask") & parameter("userId", "taskId", "fromHour", "fromUser")) { (userId, taskId, fromHour, fromUser) =>
          askFor(ActorManager.planHours, DeleteUserTask(userId, taskId, fromHour, fromUser))
        },
        (get & path("plannedHours")) {
          askFor(ActorManager.planHours, GetPlannedHours())
        },
        (post & path("consumePlanHours") & entity(as[String]) & parameter("userId", "taskId", "details")) { (planHours, userId, taskId, details) =>
          askFor(ActorManager.planHours, ConsumePlanHours(planHours, userId, taskId, details))
        },
        (get & path("consumed") & parameter("userId")) { (userId) =>
          askFor(ActorManager.plan, GetConsumedPlan(userId))
        },
        (get & path("savePlanHours") & parameter("userId", "taskId", "value", "plan")) { (userId, taskId, value, plan) =>
          askFor(ActorManager.planHours, SavePlannedHours(userId: String, taskId: String, value: String, plan: String))
        },

        (get & path("subscribeForIssue") & parameter("user") & parameter("issue") & parameter("options")) { (user, issue, options) =>
          askFor(ActorManager.issue, SubscribeForNotifications(user, issue, options))
        },
        (get & path("updateEmail") & parameter("user") & parameter("email")) { (user, email) =>
          askFor(ActorManager.auth, (user, UpdateEmail(user, email)))
        },
        (get & path("updateRocketLogin") & parameter("user") & parameter("rocketLogin")) { (user, rocketLogin) =>
          askFor(ActorManager.auth, (user, UpdateRocketLogin(user, rocketLogin)))
        },
        (get & path("notifyDocUpload") & parameter("taskId") & parameter("kind") & parameter("comment") & parameter("count")) { (taskId, kind, comment, count) =>
          askFor(ActorManager.issue, NotifyDocUpload(taskId, kind, comment, count))
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
        (post & path("createFileUrl") & entity(as[Multipart.FormData]) & parameter("user")) { (formData, user) =>
          var fileName = ""
          var fileUrl = ""
          val date = new Date().getTime
          val done: Future[Done] = formData.parts.mapAsync(1) {
            case b: BodyPart if b.name == "file" =>
              fileName = b.filename.get
              var pathId = UUID.randomUUID().toString.substring(0, 8)
              var file = new File(App.Cloud.Directory + "/" + pathId)
              while (file.exists()) {
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
        (post & path("createFileCloudUrl") & entity(as[Multipart.FormData]) & parameter("filePath") & parameter("login") & parameter("password")) { (formData, filePath, login, password) =>
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
        (post & path("uploadFile") & entity(as[Multipart.FormData]) & parameter("user")) { (formData, user) =>
          var fileName = ""
          var filePath = Path.of("")
          val done: Future[Done] = formData.parts.mapAsync(1) {
            case b: BodyPart if b.name == "file" =>
              val file = File.createTempFile("upload", "tmp")
              filePath = file.toPath
              b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
              fileName = b.filename.get
              Future.successful(Done)
            case _ => Future.successful(Done)
          }.runWith(Sink.ignore)
          onSuccess(done) { _ =>
            askFor(ActorManager.files, UploadFileToMongo(fileName, filePath, user))
          }
        },
        (get & path("file" / Segment / Segment)) { (id, name) =>
          askFor(ActorManager.files, GetFileFromMongo(id, name))
        },
        (get & path("files" / Segment / Segment)) { (path, name) =>
          getFromFile(App.Cloud.Directory + "/" + path + "/" + name)
        },
        //ISSUE MANAGER COMMANDS
        (get & path("getPdSpList")) {
          askFor(ActorManager.files, GetPdSpList())
        },
        //MATERIAL COMMANDS
        (get & path("materials") & parameter("project")) { project =>
          askFor(ActorManager.materials, GetMaterials(project))
        },
        (get & path("materialsCode") & parameter("project") & parameter("code")) { (project, code) =>
          askFor(ActorManager.materials, GetMaterialsCode(project, code))
        },
        (get & path("updateMaterial") & parameter("material") & parameter("user") & parameter("remove")) { (material, user, remove) =>
          askFor(ActorManager.materials, UpdateMaterial(material, user, remove))
        },
        (get & path("materialNodes") & parameter("project")) { project =>
          askFor(ActorManager.materials, GetMaterialNodes(project))
        },
        (get & path("updateMaterialNode") & parameter("project") & parameter("data") & parameter("label") & parameter("user") & parameter("remove")) { (project, data, label, user, remove) =>
          askFor(ActorManager.materials, UpdateMaterialNode(project, data, label, label, user, remove))
        },

        //FEST
        (post & path("createFestFileUrl") & entity(as[Multipart.FormData])) { (formData) =>
          var fileName = ""
          var fileUrl = ""
          val date = new Date().getTime
          val done: Future[Done] = formData.parts.mapAsync(1) {
            case b: BodyPart if b.name == "file" =>
              fileName = b.filename.get
              var pathId = UUID.randomUUID().toString.substring(0, 8)
              var file = new File(App.Cloud.Directory + "/" + pathId)
              while (file.exists()) {
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
        (get & path("festStories")) {
          askFor(ActorManager.fest, GetFestStories())
        },
        (get & path("setFestStories") & parameter("url") & parameter("thumb")) { (url, thumb) =>
          askFor(ActorManager.fest, SetFestStories(url, thumb))
        },
        (get & path("deleteFestStories") & parameter("url")) { (url) =>
          askFor(ActorManager.fest, DeleteFestStories(url))
        },

        (get & path("festKaraoke")) {
          askFor(ActorManager.fest, GetFestKaraoke())
        },
        (get & path("setFestKaraoke") & parameter("users") & parameter("song")) { (users, song) =>
          askFor(ActorManager.fest, SetFestKaraoke(users, song))
        },
        (get & path("deleteFestKaraoke") & parameter("time")) { (time) =>
          askFor(ActorManager.fest, DeleteFestKaraoke(time))
        },

        (get & path("festSauna")) {
          askFor(ActorManager.fest, GetFestSauna())
        },
        (get & path("setFestSauna") & parameter("kind") & parameter("users") & parameter("time")) { (kind, users, time) =>
          askFor(ActorManager.fest, SetFestSauna(kind, users, time))
        },
        (get & path("deleteFestSauna") & parameter("time")) { (time) =>
          askFor(ActorManager.fest, DeleteFestSauna(time))
        },

        (get & path("teamsWon")) {
          askFor(ActorManager.fest, GetTeamsWon())
        },
        (post & path("setTeamsWon") & entity(as[String])) { (teamsWon) =>
          askFor(ActorManager.fest, SetTeamsWon(teamsWon))
        },

        (get & path("marks")) {
          askFor(ActorManager.fest, GetMarks())
        },
        (post & path("setMarks") & entity(as[String])) { (marks) =>
          askFor(ActorManager.fest, SetMarks(marks))
        },

        (get & path("marks")) {
          askFor(ActorManager.fest, GetMarks())
        },
        (post & path("setMarks") & entity(as[String])) { (marks) =>
          askFor(ActorManager.fest, SetMarks(marks))
        },

        (get & path("bestPlayers")) {
          askFor(ActorManager.fest, GetBestPlayers())
        },
        (post & path("setBestPlayer") & entity(as[String])) { (bestPlayer) =>
          askFor(ActorManager.fest, SetBestPlayer(bestPlayer))
        },
        (get & path("sendNotificationToUser") & parameter("user") & parameter("message")) { (user, message) =>
          askFor(ActorManager.rocket, SendNotification(user, message))
        },

        (get & path("messageReactions")) {
          askFor(ActorManager.issue, GetMessageReactions())
        },
        (post & path("setMessageReaction") & entity(as[String])) { (reaction) =>
          askFor(ActorManager.issue, SetMessageReaction(reaction))
        },
        (get & path("deleteMessageReaction") & parameter("id")) { (id) =>
          askFor(ActorManager.issue, DeleteMessageReaction(id.toIntOption.getOrElse(0)))
        },
        (get & path("ping")) {
          complete(HttpEntity("pong"))
        },

        //        (post & path("grabInfo") & entity(as[String])) { (data) =>
        //          askFor(ActorManager.timeControl, AddUserWatch(data))
        //        },
        //        (post & path("spyWatch") & entity(as[String])) { (data) =>
        //          askFor(ActorManager.timeControl, AddSpyWatch(data))
        //        },
        //        (get & path("spyWatches")) {
        //          askFor(ActorManager.timeControl, GetSpyWatches())
        //        },
        //        (get & path("userWatches")) {
        //          askFor(ActorManager.timeControl, GetUserWatches())
        //        },
        (get & path("time")) {
          askFor(ActorManager.timeControl, GetTime())
        },

        (get & path("dailyTasks")) {
          askFor(ActorManager.issue, GetDailyTasks())
        },
        (post & path("addDailyTask") & entity(as[String])) { (data) =>
          askFor(ActorManager.issue, AddDailyTask(data))
        },
        (get & path("deleteDailyTask") & parameter("id")) { (id) =>
          askFor(ActorManager.issue, DeleteDailyTask(id))
        },

        (get & path("createMaterialCloudDirectory") & parameter("project") & parameter("code")) { (project, code) =>
          askFor(ActorManager.files, CreateMaterialCloudDirectory(project, code))
        },
        (get & path("createDocumentCloudDirectory") & parameter("id")) { (id) =>
          askFor(ActorManager.files, CreateDocumentCloudDirectory(id))
        },
        (get & path("cloud") & parameter("path")) { (path) =>
          askFor(ActorManager.files, GetFileFromCloud(path))
        },
        (get & path("cloudDocFiles") & parameter("id")) { (id) =>
          askFor(ActorManager.files, GetDocumentFiles(id))
        },
        (get & path("cloudFiles") & parameter("filter")) { (filter) =>
          askFor(ActorManager.files, GetCloudFiles(filter))
        },
        (get & path("cloud" / Segment) & parameter("path")) { (name, path) =>
          askFor(ActorManager.files, GetFileFromCloud(path))
        },

        //      (get & path("polygons")) {
        //        askFor(ActorManager.osmManager, GetPLS())
        //      },
        //      (post & path("savePoly") & entity(as[String])) { (poly) =>
        //        askFor(ActorManager.osmManager, AddPLS(poly))
        //      },

        (get & path("setPlanHours") & parameter("issue_id") & parameter("user") & parameter("hours")) { (issue_id, user, hours) =>
          askFor(ActorManager.issue, SetPlanHours(issue_id, user, hours))
        },
        (get & path("lockPlanHours") & parameter("issue_id") & parameter("state")) { (issue_id, state) =>
          askFor(ActorManager.issue, LockPlanHours(issue_id, state))
        },


        (get & path("plan")) {
          askFor(ActorManager.plan, GetPlan())
        },
        (get & path("planIssues") & parameter("short")) { (short) =>
          askFor(ActorManager.plan, GetPlanIssues(short.toIntOption.getOrElse(0)))
        },
        (get & path("planIssue") & parameter("id")) { (id) =>
          askFor(ActorManager.plan, GetPlanIssue(id))
        },
        (get & path("planByDays") & parameter("date")) { (date) =>
          askFor(ActorManager.plan, GetPlanByDays(date))
        },
        (get & path("planByDaysOfUser") & parameter("date") & parameter("user_id")) { (date, user_id) =>
          askFor(ActorManager.plan, GetPlanByDaysOfUser(date, user_id.toIntOption.getOrElse(0)))
        },
        (get & path("planDeleteInterval") & parameter("id", "fromUser")) { (id, fromUser) =>
          askFor(ActorManager.plan, DeleteInterval(id, fromUser))
        },
        (get & path("plan") & parameter("user") & parameter("from")) { (user, from) =>
          askFor(ActorManager.plan, GetUserPlan(user, from))
        },
        (get & path("planNotOrdinary") & parameter("from")) { (from) =>
          askFor(ActorManager.plan, GetPlanNotOrdinary(from))
        },
        (get & path("planAddInterval") & parameter("taskId", "userId", "from", "hoursAmount", "taskType", "fromUser")) { (taskId, userId, from, hoursAmount, taskType, fromUser) =>
          askFor(ActorManager.plan, AddInterval(taskId, userId, from, hoursAmount, taskType, fromUser))
        },
        (get & path("planAddIntervalAllowPast") & parameter("taskId", "userId", "from", "hoursAmount", "taskType", "fromUser")) { (taskId, userId, from, hoursAmount, taskType, fromUser) =>
          askFor(ActorManager.plan, AddIntervalAllowPast(taskId, userId, from, hoursAmount, taskType, fromUser))
        },
        (get & path("planInsertInterval") & parameter("taskId", "userId", "from", "hoursAmount", "taskType", "fromUser")) { (taskId, userId, from, hoursAmount, taskType, fromUser) =>
          askFor(ActorManager.plan, InsertInterval(taskId, userId, from, hoursAmount, taskType, fromUser))
        },
        (get & path("planInsertConsumedInterval") & parameter("taskId", "userId", "from", "hoursAmount", "taskType")) { (taskId, userId, from, hoursAmount, taskType) =>
          askFor(ActorManager.plan, InsertConsumedInterval(taskId, userId, from, hoursAmount, taskType))
        },
        (get & path("addManHours") & parameter("taskId", "userId", "dateConsumed", "hoursAmount", "message")) { (taskId, userId, dateConsumed, hoursAmount, message) =>
          askFor(ActorManager.plan, AddManHours(taskId, userId, dateConsumed, hoursAmount, message))
        },
        (get & path("userDiary") & parameter("userId")) { (userId) =>
          askFor(ActorManager.plan, GetUserDiary(userId))
        },
        (get & path("deleteFromDiary") & parameter("id")) { (id) =>
          askFor(ActorManager.plan, DeleteFromDiary(id))
        },

        (post & path("statsUsersDetails") & entity(as[String]) & parameter("dateFrom", "dateTo")) { (users, dateFrom, dateTo) =>
          askFor(ActorManager.plan, GetUserStats(dateFrom, dateTo, users))
        },
        (get & path("projectStats") & parameter("project", "docType")) { (project, docType) =>
          askFor(ActorManager.plan, GetProjectStats(project, docType))
        },

        (get & path("addMaterialComplect") & parameter("project", "name", "kind", "user_id")) { (project, name, kind, user_id) =>
          askFor(ActorManager.materials, AddMaterialComplect(project, name, kind, user_id))
        },
        (get & path("removeMaterialComplect") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, RemoveMaterialComplect(id))
        },
        (get & path("materialComplects") & parameter("project")) { (project) =>
          askFor(ActorManager.materials, GetMaterialComplects(project))
        },
        (post & path("updateMaterialComplect") & entity(as[String])) { (complectValue) =>
          askFor(ActorManager.materials, UpdateMaterialComplect(complectValue))
        },

        (get & path("equipments")) {
          askFor(ActorManager.materials, GetEquipments())
        },
        (get & path("equipment") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, GetEquipment(id))
        },
        (post & path("equipment") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, InsertEquipment(jsonValue))
        },
        (get & path("deleteEquipment") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, DeleteEquipment(id.toIntOption.getOrElse(0)))
        },
        (get & path("sfis")) {
          askFor(ActorManager.materials, GetSFIs())
        },
        (post & path("supplier") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, InsertSupplier(jsonValue))
        },
        (get & path("deleteSupplier") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, DeleteSupplier(id.toIntOption.getOrElse(0)))
        },

        (get & path("equipFiles") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, GetEquipFiles(id.toIntOption.getOrElse(0)))
        },
        (post & path("addEquipFile") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, AddEquipFile(jsonValue))
        },
        (get & path("delEquipFile") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, DelEquipFile(id.toIntOption.getOrElse(0)))
        },

        (get & path("supFiles") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, GetSupFiles(id.toIntOption.getOrElse(0)))
        },
        (post & path("addSupFile") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, AddSupFile(jsonValue))
        },
        (get & path("delSupFile") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, DelSupFile(id.toIntOption.getOrElse(0)))
        },

        (get & path("relatedTasks") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, GetRelatedTasks(id.toIntOption.getOrElse(0)))
        },
        (get & path("delRelatedTask") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, DelRelatedTask(id.toIntOption.getOrElse(0)))
        },

        (get & path("supHistory") & parameter("id")) { (id) =>
          askFor(ActorManager.materials, GetSupplierHistory(id.toIntOption.getOrElse(0)))
        },
        (post & path("addSupHistory") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, AddSupplierHistory(jsonValue))
        },
        (get & path("specMaterials")) {
          askFor(ActorManager.materials, GetSpecMaterials())
        },
        (get & path("specDirectories")) {
          askFor(ActorManager.materials, GetSpecDirectories())
        },
        (get & path("specStatements")) {
          askFor(ActorManager.materials, GetSpecStatements())
        },
        (get & path("supStatuses")) {
          askFor(ActorManager.materials, GetSupStatuses())
        },
        (post & path("specMaterial") & entity(as[String])) {  (jsonValue) =>
          askFor(ActorManager.materials, UpdateSpecMaterial(jsonValue))
        },
        (post & path("specDirectory") & entity(as[String])) {  (jsonValue) =>
          askFor(ActorManager.materials, UpdateMaterialDirectory(jsonValue))
        },
        (get & path("supNames")) {
          askFor(ActorManager.materials, GetSupNames())
        },
        (post & path("addSupName") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, AddSupName(jsonValue))
        },
        (get & path("supMatRelations")) {
          askFor(ActorManager.materials, GetSupMatRelations())
        },
        (post & path("addSupMatRelations") & entity(as[String])) { (jsonValue) =>
          askFor(ActorManager.materials, AddSupMatRelation(jsonValue))
        },
        (get & path("eqSupMatRelations") & parameter("supId")) { (supId) =>
          askFor(ActorManager.materials, GetEqSupMatRelations(supId))
        },
        (post & path("supTaskAdd") & entity(as[String])) { (supTask) =>
          askFor(ActorManager.materials, SupTaskAdd(supTask))
        },

        (withoutSizeLimit & post & path("store-file") & entity(as[Multipart.FormData])){ (formData) =>
          var fileName = ""
          var fileUrl = ""
          val done: Future[Done] = formData.parts.mapAsync(1) {
            case b: BodyPart if b.name == "file" =>
              fileName = b.filename.get
              var pathId = UUID.randomUUID().toString.substring(0, 8)
              val c = Calendar.getInstance()
              val date = List(c.get(Calendar.DAY_OF_MONTH), c.get(Calendar.MONTH) + 1, c.get(Calendar.YEAR)).mkString("-")
              var path = Paths.get(App.Cloud.Directory, date, pathId).toString
              var file = new File(path)
              while (file.exists()){
                pathId = UUID.randomUUID().toString.substring(0, 8)
                path = Paths.get(App.Cloud.Directory, date, pathId).toString
                file = new File(path)
              }
              file.mkdirs()
              file = new File(Paths.get(App.Cloud.Directory, date, pathId, fileName).toString)
              fileUrl = App.HTTPServer.RestUrl + "/s-files/" + date + "/" + pathId + "/" + fileName
              b.entity.dataBytes.runWith(FileIO.toPath(file.toPath))
              Future.successful(Done)
            case _ => Future.successful(Done)
          }.runWith(Sink.ignore)
          onSuccess(done) { _ =>
            complete(HttpEntity(fileUrl.asJson.noSpaces))
          }
        },
        (get & path("s-files" / Segment / Segment / Segment)){ (date, pathId, name) =>
          getFromFile(App.Cloud.Directory + "/" + date + "/" + pathId + "/" + name)
        },
        (get & path("s-files-download" / Segment  / Segment / Segment)){ (date, pathId, name) =>
          getFromFile(new File(App.Cloud.Directory + "/" + date + "/" + pathId + "/" + name), ContentTypes.NoContentType)
        },
        (get & path("storageUnits")) {
          askFor(ActorManager.storage, GetStorageUnits())
        },
        (get & path("newStorageUnit")) {
          askFor(ActorManager.storage, GetNewStorageUnit())
        },
        (post & path("updateStorageUnit") & entity(as[String])) { (storageUnit) =>
          askFor(ActorManager.storage, UpdateStorageUnit(storageUnit))
        },
        (get & path("storageFiles")) {
          askFor(ActorManager.storage, GetStorageFiles())
        },
        (get & path("storageLocations")) {
          askFor(ActorManager.storage, GetStorageLocations())
        },
        (post & path("updateStorageLocation") & entity(as[String])) { (json) =>
          askFor(ActorManager.storage, UpdateStorageLocation(json))
        },
        (post & path("updateStorageFile") & entity(as[String])) { (storageFile) =>
          askFor(ActorManager.storage, UpdateStorageFile(storageFile))
        },

        (get & path("materialCheck")) {
          askFor(ActorManager.materials, GetMaterialCheck())
        },
        (get & path("materialCheckAdd") & parameter("value")) { (value) =>
          askFor(ActorManager.materials, AddMaterialCheck(value))
        },
        (get & path("materialCheckDelete") & parameter("value")) {  (value) =>
          askFor(ActorManager.materials, DeleteMaterialCheck(value))
        },
      )
    }
  }

  def askFor(actor: ActorRef, command: Any, long: Boolean = false): Route = {
    onComplete(actor.ask(command)) {
      case Success(value) => value match {
        case response: JsValue => complete(HttpEntity(response.toString()))
        case response: Array[Byte] => complete(HttpEntity(response))
        case response: String => complete(HttpEntity(response))
        case response: io.circe.Json => complete(HttpEntity(response.noSpaces))
        case response: HttpEntity.Strict => complete(response)
        case response: File => getFromFile(response)
        case response: MongoFile =>
          val resolver = ContentTypeResolver.Default

          val contentType: ContentType = resolver.resolve(response.fileName).asInstanceOf[ContentType]
          complete(HttpEntity(contentType, response.bytes))
        case _ => complete(HttpEntity(Json.toJson("Error: Wrong response from actor.").toString()))
      }
      case Failure(exception) => complete(HttpEntity(exception.toString))
    }
  }

  def error: StandardRoute = complete(HttpEntity(Json.toJson("Error: Wrong response.").toString()))

  override def preStart(): Unit = {
    server = Http().newServerAt(App.HTTPServer.Host, App.HTTPServer.Port).bind(routes)
    logger.error("HTTP server has been started at " + App.HTTPServer.Host + " with port " + App.HTTPServer.Port)
    ActorManager.startup ! HTTPManagerStarted()
  }

  override def postStop(): Unit = {
    server.flatMap(_.unbind()).onComplete(_ => system.terminate())
  }

  override def receive: Receive = {
    case _ => None
  }

  private def logIp(ip: RemoteAddress): Unit = {
    ip.toOption.map(_.getHostAddress) match {
      case Some(value) =>
        if (!value.equals("192.168.1.1")) {
          println(value + ", " + new Date().toLocaleString)
        }
      case None => println("unknown ip, " + new Date().toLocaleString)
    }
  }
}
