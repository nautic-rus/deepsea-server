package deepsea.issues

import akka.actor.{Actor, actorRef2Scala}
import akka.pattern.ask
import akka.util.Timeout
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.{GetUser, User}
import deepsea.database.{DBManager, DatabaseManager, MongoCodecs}
import deepsea.database.DatabaseManager.GetConnection
import deepsea.files.FileManager.{GetCloudFiles, GetDocumentFiles, TreeFile, treeFilesCollection}
import deepsea.files.FileManagerHelper
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManager._
import deepsea.issues.classes.{ChildIssue, Issue, IssueAction, IssueCheck, IssueHistory, IssueMessage, IssuePeriod, IssueType, IssueView, SfiCode}
import deepsea.rocket.RocketChatManager.SendNotification
import deepsea.time.TimeControlManager.UserWatch
import org.mongodb.scala.{Document, MongoCollection}
import play.api.libs.json.{JsValue, Json, OWrites}
import io.circe.parser._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.model.Filters.{and, equal}

import java.util.Date
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.Try

object IssueManager{

  case class GetIssues(user: String)
  case class GetQuestions()
  case class StartIssue(issueJson: String)
  case class UpdateIssue(user: String, updateMessage: String, issueJson: String)
  case class RemoveIssue(id: String, user: String)
  case class GetIssueProjects()
  case class GetIssueTypes()
  case class GetIssuePriorities()
  case class GetIssueDepartments()
  case class GetIssueMessages(id: String)
  case class GetIssueDetails(id: String)
  case class SetIssueStatus(id: String, user: String)
  case class SetIssueMessage(id: String, message: String)
  case class AddIssueMessage(id: String, message: IssueMessage)
  case class AssignIssue(id: String, user: String, start_date: String, due_date: String, overtime: String, action: String, author: String)
  case class ChangeResponsible(id: String, user: String, author: String, action: String)
  case class SendToApproval(id: String, users: String, filesToApproval: String, textToApproval: String, taskStatus: String, taskStatusApproval: String, taskTypeApproval: String, taskRevision: String)
  case class SetIssueViewed(id: String, user: String)
  case class GetIssuesViewed(user: String)
  case class GetIssueId()
  case class SetDayCalendar(user: String, day: String, status: String)
  case class GetCalendar()
  case class DeleteFile(url: String)
  case class GetIssuePeriods()
  case class SetRevisionFiles(id: String, revision: String, filesJson: String)
  case class DeleteRevisionFile(file_url: String, user: String)
  case class ClearRevisionFiles(issueId: String, user: String, fileGroup: String, revision: String)
  case class GetRevisionFiles()
  case class SetIssueLabor(user: String, issue_id: String, labor_value: String, labor_comment: String, date: String)
  case class GetSfiCodes()
  case class GetIssueSpentTime()
  case class GetIssueChecks(issue_id: String)
  case class GetCheckTemplates(user: String)
  case class SetIssueChecks(issue_id: String, checks: String)
  case class UpdateIssueCheck(issue_id: String, user: String, check_description: String, check_group: String, check_status: String)
  case class GetNestingFiles()
  case class GetAmountTask(project: String, department: String, status: String)
  case class MessageReaction(message_id: Int, reaction_icon: String, user_reacted: String, date_reacted: Long)
  case class GetMessageReactions()
  case class SetMessageReaction(jsValue: String)
  case class DeleteMessageReaction(id: Int)
  case class GetDailyTasks()
  case class AddDailyTask(jsValue: String)
  case class DeleteDailyTask(id: String)
  case class CombineIssues(firstIssue: String, secondIssue: String, user: String)
  case class GetProjectNames()

  case class IssueDef(id: String, issueTypes: List[String], issueProjects: List[String])
  implicit val writesIssueDef: OWrites[IssueDef] = Json.writes[IssueDef]


  case class IdName(id: Int, name: String)
  implicit val writesUser: OWrites[IdName] = Json.writes[IdName]

  case class DayCalendar(user: String, day: String, status: String)
  implicit val writesDayCalendar: OWrites[DayCalendar] = Json.writes[DayCalendar]

  case class IssueSpentTime(id: Int, date: Long, value: Double, comment: String, user: String)
  implicit val writesIssueSpentTime: OWrites[IssueSpentTime] = Json.writes[IssueSpentTime]

  case class GroupFolder(id: Int, name: String)
  case class DailyTask(date: Long, userLogin: String, userName: String, dateCreated: Long, project: String, docNumber: String, details: String, time: Double, action: String, id: String)
  case class IssueProject(id: Int, name: String, pdsp: String, rkd: String, foran: String, factory: String)
}
class IssueManager extends Actor with MongoCodecs with IssueManagerHelper with FileManagerHelper {
  implicit val timeout: Timeout = Timeout(30, TimeUnit.SECONDS)

  override def preStart(): Unit = {
//    ActorManager.files ! GetDocumentFiles(602.toString)
//    self ! GetIssues("op")
//    self ! GetIssueDetails(1272.toString)
//    DBManager.GetNextCloudConnection() match {
//      case Some(connection) =>
//        val stmt = connection.createStatement()
//        val query = "select * from oc_activity where file like '%0101_revB.pdf%'"
//        val rs = stmt.executeQuery(query)
//        while (rs.next()){
//          val jk = rs.getString("user")
//          val jkk = jk
//        }
//      case _ => None
//    }
//    Await.result(ActorManager.auth ? GetUser("op"), timeout.duration) match {
//      case user: User =>
//        val issues = getIssuesForUser(user)
//        val revisionFiles = getRevisionFiles.filter(_.removed_date == 0)
//        val issuesFiltered = issues.filter(x => List("NR002").contains(x.project) && x.issue_type == "RKD")
//        issuesFiltered.foreach(issue => {
//          val files = revisionFiles.filter(_.issue_id == issue.id)
//          if (files.nonEmpty){
//            getIssueDetails(issue.id) match {
//              case Some(details) =>
//                copyFilesToDirectory(details.doc_number, details.department, files.toList, "C:\\documents")
//              case _ => None
//            }
//          }
//          val jkk = issue
//        })
//        val jk = 0
//
//
//      case _ => sender() ! Json.toJson(ListBuffer.empty[Issue])
//    }
  }
  override def receive: Receive = {
    case GetIssueProjects() => sender() ! getIssueProjects.asJson.noSpaces
    case GetIssueTypes() => sender() ! Json.toJson(getIssueTypes)
    case GetIssueDepartments() => sender() ! Json.toJson(getIssueDepartments)
    case GetIssuePriorities() => sender() ! Json.toJson(getIssuePriorities)
    case StartIssue(issueJson) =>
      Json.parse(issueJson).asOpt[Issue] match {
        case Some(issue) =>
          val result = startIssue(issue)
          issue.file_attachments.foreach(x => setIssueFileAttachments(result, x))
          if (issue.assigned_to != ""){
            ActorManager.rocket ! SendNotification(issue.assigned_to, s"Вам была назначена задача " + s"<${App.HTTPServer.Url}/?taskId=${result}| Просмотреть задачу>")
          }
          if (issue.responsible != ""){
            ActorManager.rocket ! SendNotification(issue.responsible, s"Вы были назначены ответственным к задаче " + s"<${App.HTTPServer.Url}/?taskId=${result}| Просмотреть задачу>")
          }
          sender() ! Json.toJson(result)
        case _ => None
      }
    case GetIssues(userName) =>
      Await.result(ActorManager.auth ? GetUser(userName), timeout.duration) match {
        case user: User =>
          sender() ! Json.toJson(getIssuesForUser(user))
        case _ => sender() ! Json.toJson(ListBuffer.empty[Issue])
      }
    case GetQuestions() =>
      sender() ! Json.toJson(getIssuesForUser(user))
    case UpdateIssue(user, updateMessage, issueJson) =>
      Json.parse(issueJson).asOpt[Issue] match {
        case Some(issue) =>
          updateIssue(issue, user, updateMessage)
          sender() ! (getIssueDetails(issue.id) match {
            case Some(update) =>
              if (updateMessage.contains("status")){
                List(update.assigned_to, update.responsible, update.started_by).filter(_ != user).distinct.foreach(u => {
                  ActorManager.rocket ! SendNotification(u, s"Изменился статус на '${update.status}' у задачи " + s"<${App.HTTPServer.Url}/?taskId=${issue.id}|${(issue.doc_number + " " + issue.name).trim}>")
                })
              }
              else if (updateMessage.contains("edit")){
                List(update.assigned_to, update.responsible, update.started_by).filter(_ != user).distinct.foreach(u => {
                  ActorManager.rocket ! SendNotification(u, s"Изменилась информация в задаче " + s"<${App.HTTPServer.Url}/?taskId=${issue.id}|${(issue.doc_number + " " + issue.name).trim}>")
                })
              }
              else {
                List(update.assigned_to, update.responsible, update.started_by).filter(_ != user).distinct.foreach(u => {
                  ActorManager.rocket ! SendNotification(u, s"Что-то поменялось в задаче " + s"<${App.HTTPServer.Url}/?taskId=${issue.id}|${(issue.doc_number + " " + issue.name).trim}>")
                })
              }
              Json.toJson(update)
            case _ => "error"
          })
        case _ =>
          sender() ! Json.toJson("error")
      }
    case RemoveIssue(_id, user) =>
      val id = Try(_id.toInt).getOrElse(0)
      removeIssue(id, user)
      sender() ! Json.toJson("success")
    case GetIssueDetails(_id) =>
      sender() ! (getIssueDetails(Try(_id.toInt).getOrElse(0)) match {
        case Some(issue) => Json.toJson(issue)
        case _ => "issue not found"
      })
    case AssignIssue(_id, user, _start_date, _due_date, overtime, action, author) =>
      val id: Int = _id.toIntOption.getOrElse(0)
      val start_date: Long = _start_date.toLongOption.getOrElse(0)
      val due_date: Long = _due_date.toLongOption.getOrElse(0)
      assignIssue(id, user, start_date, due_date, overtime, action, author)
      ActorManager.rocket ! SendNotification(user, s"Вам была назначена задача " + s"<${App.HTTPServer.Url}/?taskId=${id}|Просмотреть задачу>")
      sender() ! Json.toJson("success")
    case ChangeResponsible(_id, user, author, action) =>
      val id = Try(_id.toInt).getOrElse(0)
      changeResponsible(id, user, author, action)
      sender() ! Json.toJson("success")
    case SetIssueMessage(_id, message) =>
      Json.parse(message).asOpt[IssueMessage] match {
        case Some(msg) =>
          val id = _id.toIntOption.getOrElse(0)
          val messageId = setIssueMessage(id, msg)
          msg.file_attachments.foreach(x => setMessageFileAttachments(messageId, x))
          //msg.fileAttachments.foreach(x => setIssueFileAttachments(id, x))
//          ActorManager.rocket ! SendNotification(msg.author, s"Появилось новое сообщение к задаче " + s"<${App.HTTPServer.Url}/?taskId=${id} | Просмотреть задачу>")
          sender() ! Json.toJson("success")
        case _ =>
          sender() ! Json.toJson("error")
      }
    case SetIssueViewed(_id, user) =>
      setIssueViewed(Try(_id.toInt).getOrElse(0), user)
      sender() ! Json.toJson("success")
    case GetIssuesViewed(user) =>
      sender() ! Json.toJson(getIssueViews(user))
    case GetIssueId() => sender() ! getIssueId
    case GetCalendar() => sender() ! Json.toJson(getCalendar)
    case SetDayCalendar(user, day, status) =>
      setDayCalendar(user, day, status)
      sender() ! Json.toJson("success")
    case DeleteFile(url) =>
      deleteFile(url)
      sender() ! Json.toJson("success")
    case GetIssuePeriods() =>
      sender() ! Json.toJson(getIssuePeriods)
    case SetRevisionFiles(_id, revision, filesJson) =>
      setRevisionFiles(_id.toIntOption.getOrElse(0), revision, Json.parse(filesJson).asOpt[ListBuffer[FileAttachment]].getOrElse(ListBuffer.empty[FileAttachment]))
      sender() ! Json.toJson("success")
    case DeleteRevisionFile(file_url, user) =>
      deleteRevisionFile(file_url, user)
      sender() ! Json.toJson("success")
    case ClearRevisionFiles(issueId, user, fileGroup, revision) =>
      clearRevisionFiles(issueId.toIntOption.getOrElse(0), user, fileGroup, revision)
      sender() ! Json.toJson("success")
    case GetRevisionFiles() =>
      sender() ! Json.toJson(getRevisionFiles)
    case SetIssueLabor(user, issue_id, labor_value, labor_comment, date) =>
      setIssueLabor(user, issue_id.toIntOption.getOrElse(0), labor_value.toDoubleOption.getOrElse(0), labor_comment, date.toLongOption.getOrElse(0))
      sender() ! Json.toJson("success")
    case GetSfiCodes() =>
      sender() ! Json.toJson(getSfiCodes)
    case GetIssueSpentTime() =>
      sender() ! Json.toJson(getIssueSpentTime)
    case GetIssueChecks(issue_id) =>
      sender() ! Json.toJson(getIssueChecks(issue_id.toIntOption.getOrElse(0)))
    case SetIssueChecks(issue_id, checks: String) =>
      setIssueChecks(issue_id.toIntOption.getOrElse(0), Json.parse(checks).asOpt[List[IssueCheck]].getOrElse(List.empty[IssueCheck]))
      sender() ! Json.toJson("success")
    case UpdateIssueCheck(issue_id, user, check_description, check_group, check_status) =>
      updateIssueCheck(issue_id.toIntOption.getOrElse(0), user, check_description, check_group, check_status.toIntOption.getOrElse(0))
      sender() ! Json.toJson("success")
    case GetCheckTemplates(user) =>
      sender() ! Json.toJson(getCheckTemplates(user))
    case GetNestingFiles() =>
      sender() ! Json.toJson(getNestingRevisionFiles)
    case GetAmountTask(project, departments, status) => None
    //request amount of task
    //sender() ! Json.toJson(getAmountTask(project, departments, status))
    case GetMessageReactions() => getMessageReactions.asJson.noSpaces
    case SetMessageReaction(jsValue: String) => setMessageReaction(jsValue)
    case DeleteMessageReaction(id: Int) =>
      deleteMessageReaction(id)
      sender() ! "success".asJson.noSpaces

    case GetDailyTasks() =>
      sender() ! getDailyTasks.asJson.noSpaces
    case AddDailyTask(jsValue) =>
      addDailyTask(jsValue)
      sender() ! "success".asJson.noSpaces
    case DeleteDailyTask(id) =>
      deleteDailyTask(id)
      sender() ! "success".asJson.noSpaces
    case CombineIssues(firstIssue, secondIssue, user) =>
      combineIssues(firstIssue.toIntOption.getOrElse(0), secondIssue.toIntOption.getOrElse(0), user)
      sender() ! "success".asJson.noSpaces
    case GetProjectNames() =>
      sender() ! getProjectNames.asJson.noSpaces
    case _ => None
  }
  def setDayCalendar(user: String, day: String, status: String): ListBuffer[IssueView] ={
    val res = ListBuffer.empty[IssueView]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        val already = s.executeQuery(s"select * from days_calendar where day = '$day' and user_login = '$user'")
        if (already.next()){
          s.execute(s"update days_calendar set status = '$status' where day = '$day' and user_login = '$user'")
        }
        else{
          s.execute(s"insert into days_calendar values ('$user', '$day', '$status')")
        }
        already.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getCalendar: ListBuffer[DayCalendar] ={
    val res = ListBuffer.empty[DayCalendar]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from days_calendar")
        while (rs.next()){
          res += DayCalendar(rs.getString("user_login"), rs.getString("day"), rs.getString("status"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setIssueViewed(id: Int, user: String): ListBuffer[IssueView] ={
    val res = ListBuffer.empty[IssueView]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        val already = s.executeQuery(s"select * from issue_viewed where issue = $id and user_login = '$user'")
        if (already.next()){
          s.execute(s"update issue_viewed set update_date = $date where issue = $id and user_login = '$user'")
        }
        else{
          s.execute(s"insert into issue_viewed values ('$user', $id, $date)")
        }
        already.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueViews(user: String): ListBuffer[IssueView] ={
    val res = ListBuffer.empty[IssueView]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_viewed where user_login = '$user'")
        while (rs.next()){
          res += new IssueView(rs.getString("user_login"), rs.getInt("issue"), rs.getLong("update_date"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueId: Int = {
    var res = 0
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery("select nextval('users_id_seq')")
        while (rs.next()){
          res = rs.getInt("nextval")
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueProjects: ListBuffer[IssueProject] ={
    val res = ListBuffer.empty[IssueProject]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_projects order by id")
        while (rs.next()){
          res += IssueProject(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getString("name")).getOrElse(""),
            Option(rs.getString("pdsp")).getOrElse(""),
            Option(rs.getString("rkd")).getOrElse(""),
            Option(rs.getString("foran")).getOrElse(""),
            Option(rs.getString("factory")).getOrElse("")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueDepartments: ListBuffer[String] ={
    val res = ListBuffer.empty[String]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_departments order by id")
        while (rs.next()){
          res += rs.getString("name")
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssuePriorities: ListBuffer[String] ={
    val res = ListBuffer.empty[String]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_priorities")
        while (rs.next()){
          res += rs.getString("name")
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setIssueMessage(id: Int, message: IssueMessage): Int ={
    var result = 0
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"insert into issue_messages (issue_id, author, content, date, prefix, to_be_replied) values ($id, '${message.author}', '${message.content}', ${new Date().getTime}, '${message.prefix}', ${message.to_be_replied}) returning id")
        while (rs.next()){
          result = rs.getInt("id")
        }
        rs.close()
        c.close()
      case _ =>
    }
    result
  }
  def updateIssueMessage(id: Int, message: IssueMessage): Int ={
    var result = 0
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update issue_messages set content = '${message.content}' where id = $id")
        c.close()
      case _ =>
    }
    result
  }
  def setIssueFileAttachments(id: Int, file: FileAttachment): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into file_attachments (issue_id, file_name, url, upload_date, author) values ($id, '${file.name}', '${file.url}', $date, '${file.author}')")
        c.close()
      case _ =>
    }
  }
  def setMessageFileAttachments(id: Int, file: FileAttachment): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into file_attachments (message_id, file_name, url, upload_date, author) values ($id, '${file.name}', '${file.url}', $date, '${file.author}')")
        c.close()
      case _ =>
    }
  }
  def getIssueTypes: ListBuffer[IssueType] ={
    val res = ListBuffer.empty[IssueType]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_types order by sort")
        while (rs.next()){
          res += new IssueType(
            rs.getString("type_name"),
            rs.getString("local_approval"),
            rs.getString("yard_approval"),
            rs.getInt("sort"),
            rs.getString("visible_row"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def startIssue(issue: Issue): Int ={
    var res = 0
    val date = new Date().getTime
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"insert into issue (id, status, project, department, started_by, started_date, issue_type, issue_name, assigned_to, details, priority, last_update, doc_number, responsible, period, parent_id, active_action) " +
          s"values (default, '${issue.status}', '${issue.project}', '${issue.department}', '${issue.started_by}', $date, '${issue.issue_type}', '${issue.name}', '${issue.assigned_to}', '${issue.details}', '${issue.priority}', $date, '${issue.doc_number}', '${issue.responsible}', '${issue.period}', '${issue.parent_id}', '${issue.action}')" +
          s" returning id"
        val rs = s.executeQuery(query)
        while (rs.next()){
          res = rs.getInt("id")
        }
        rs.close()
        c.close()
        res
      case _ => res
    }
  }
  def updateHistory(issue: IssueHistory): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into issue_history values ('${issue.id}', '${issue.author}', '${issue.name_value}', '${issue.prev_value}', '${issue.new_value}', ${issue.update_date}, '${issue.update_message}')")
        c.close()
      case _ => None
    }
  }
  def removeIssue(id: Int, user: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update issue set removed = 1 where id = $id")
        c.close()
        updateHistory(new IssueHistory(id, user, "removed", "0", "1", new Date().getTime, "remove"))
      case _ => None
    }
  }
  def changeResponsible(id: Int, user: String, action: String, author: String): Unit ={
    val date = new Date().getTime
    updateHistory(new IssueHistory(id, author, "change_responsible", "", user, date, "change_responsible"))
    val query = s"update issue set responsible = '$user', action = '$action' where id = $id"
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.executeQuery(query)
        c.close()
        sender() ! Json.toJson("success")
      case _ => sender() ! Json.toJson("error")
    }
  }
  def assignIssue(id: Int, user: String, start_date: Long, due_date: Long, overtime: String, action: String, author: String): Unit ={
    val date = new Date().getTime
    updateHistory(new IssueHistory(id, author, "assign", "", user, date, "assign"))
    val query = s"update issue set assigned_to = '$user', active_action = '$action', start_date = $start_date, due_date = $due_date, overtime = '$overtime' where id = $id"
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(query)
        c.close()
        sender() ! Json.toJson("success")
      case _ => sender() ! Json.toJson("error")
    }
  }
  def updateIssue(issue: Issue, user: String, updateMessage: String): Unit ={
    val date = new Date().getTime
    var name_value = ""
    var prev_value: Any = ""
    var new_value: Any = ""
    val id = issue.id
    getIssueDetails(id) match {
      case Some(oldIssue) =>
        if (oldIssue.status != issue.status){
          name_value = "status"
          prev_value = oldIssue.status
          new_value = issue.status
        }
        else if (oldIssue.project != issue.project){
          name_value = "project"
          prev_value = oldIssue.project
          new_value = issue.project
        }
        else if (oldIssue.department != issue.department){
          name_value = "department"
          prev_value = oldIssue.department
          new_value = issue.department
        }
        else if (oldIssue.started_by != issue.started_by){
          name_value = "started_by"
          prev_value = oldIssue.started_by
          new_value = issue.started_by
        }
        else if (oldIssue.started_date != issue.started_date){
          name_value = "started_date"
          prev_value = oldIssue.started_date
          new_value = issue.started_date
        }
        else if (oldIssue.issue_type != issue.issue_type){
          name_value = "issue_type"
          prev_value = oldIssue.issue_type
          new_value = issue.issue_type
        }
        else if (oldIssue.name != issue.name){
          name_value = "issue_name"
          prev_value = oldIssue.name
          new_value = issue.name
        }
        else if (oldIssue.details != issue.details){
          name_value = "details"
          prev_value = oldIssue.details
          new_value = issue.details
        }
        else if (oldIssue.due_date != issue.due_date){
          name_value = "due_date"
          prev_value = oldIssue.due_date
          new_value = issue.due_date
        }
        else if (oldIssue.priority != issue.priority){
          name_value = "priority"
          prev_value = oldIssue.priority
          new_value = issue.priority
        }
        else if (oldIssue.doc_number != issue.doc_number){
          name_value = "doc_number"
          prev_value = oldIssue.doc_number
          new_value = issue.doc_number
        }
        else if (oldIssue.responsible != issue.responsible){
          name_value = "responsible"
          prev_value = oldIssue.responsible
          new_value = issue.responsible
        }
        else if (oldIssue.overtime != issue.overtime){
          name_value = "overtime"
          prev_value = oldIssue.overtime
          new_value = issue.overtime
        }
        else if (oldIssue.start_date != issue.start_date){
          name_value = "start_date"
          prev_value = oldIssue.start_date
          new_value = issue.start_date
        }
        else if (oldIssue.period != issue.period){
          name_value = "period"
          prev_value = oldIssue.period
          new_value = issue.period
        }
        else if (oldIssue.issue_comment != issue.issue_comment){
          name_value = "issue_comment"
          prev_value = oldIssue.issue_comment
          new_value = issue.issue_comment
        }
        else if (oldIssue.revision != issue.revision){
          name_value = "revision"
          prev_value = oldIssue.revision
          new_value = issue.revision
        }
        else if (oldIssue.assigned_to != issue.assigned_to){
          name_value = "assigned_to"
          prev_value = oldIssue.assigned_to
          new_value = issue.assigned_to
        }
        else if (oldIssue.delivered_date != issue.delivered_date){
          name_value = "delivered_date"
          prev_value = oldIssue.delivered_date
          new_value = issue.delivered_date
        }
        else if (oldIssue.first_send_date != issue.first_send_date){
          name_value = "first_send_date"
          prev_value = oldIssue.first_send_date
          new_value = issue.first_send_date
        }
        else if (oldIssue.revision != issue.revision){
          name_value = "revision"
          prev_value = oldIssue.revision
          new_value = issue.revision
        }
        else if (oldIssue.ready != issue.ready){
          name_value = "ready"
          prev_value = oldIssue.ready
          new_value = issue.ready
        }
        else if (oldIssue.contract_due_date != issue.contract_due_date){
          name_value = "contract_due_date"
          prev_value = oldIssue.contract_due_date
          new_value = issue.contract_due_date
        }
      case _ => None
    }
    if (name_value != ""){
      updateHistory(new IssueHistory(id, user, name_value, prev_value.toString, new_value.toString, date, updateMessage))
      val numeric_names = List("started_date", "due_date", "start_date", "first_send_date", "delivered_date", "contract_due_date")
      var query = s"update issue set $name_value = '$new_value', active_action = '${issue.action}', last_update = $date where id = $id"
      if (numeric_names.contains(name_value)){
        query = s"update issue set $name_value = $new_value, active_action = '${issue.action}', last_update = $date where id = $id"
      }
      GetConnection() match {
        case Some(c) =>
          val s = c.createStatement()
          s.execute(query)
          c.close()
        case _ => None
      }
      //updateIssue(issue, user, updateMessage)
    }
  }

  def deleteFile(url: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"delete from file_attachments where url = '$url'"
        s.execute(query)
        s.close()
        c.close()
      case _ =>
    }
  }
  def deleteRevisionFile(url: String, user: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val date = new Date().getTime
        val s = c.createStatement()
        val query = s"update revision_files set removed = 1, removed_by = '$user', removed_date = $date where file_url = '$url'"
        s.execute(query)
        s.close()
        c.close()
      case _ =>
    }
  }
  def clearRevisionFiles(issueId: Int, user: String, fileGroup: String, revision: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val date = new Date().getTime
        val s = c.createStatement()
        val query = s"update revision_files set removed = 1, removed_by = '$user', removed_date = $date where issue_id = $issueId and group_name = '$fileGroup' and issue_revision = '$revision'"
        s.execute(query)
        s.close()
        c.close()
      case _ =>
    }
  }
  def setRevisionFiles(id: Int, revision: String, files: ListBuffer[FileAttachment]): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        files.foreach(file => {
          s.execute(s"insert into revision_files (issue_id, file_name, file_url, upload_date, upload_user, issue_revision, group_name) values ($id, '${file.name}', '${file.url}', $date, '${file.author}', '$revision', '${file.group}')")
        })
        c.close()
      case _ =>
    }
  }
  def getRevisionFiles: ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from revision_files where removed = 0")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("file_name"),
            rs.getString("file_url"),
            rs.getLong("upload_date"),
            rs.getString("upload_user"),
            rs.getString("issue_revision"),
            rs.getString("group_name"),
          ){
            issue_id = rs.getInt("issue_id")
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }

  def combineIssues(issue_first: Int, issue_second: Int, user: String): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into issue_combined values ($issue_first, $issue_second, '$user', $date)")
        s.close()
        c.close()
      case _ =>
    }
  }
  def getNestingRevisionFiles: ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from revision_files where (group_name like 'Nesting%' or group_name like 'Cutting%') and removed = 0")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("file_name"),
            rs.getString("file_url"),
            rs.getLong("upload_date"),
            rs.getString("upload_user"),
            rs.getString("issue_revision"),
            rs.getString("group_name"),
          ){
            issue_id = rs.getInt("issue_id")
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setIssueLabor(user: String, issue_id: Int, labor_value: Double, labor_comment: String, date: Long): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into issue_spent_time (issue_id, labor_date, labor_value, labor_comment, user_labor) values ($issue_id, $date, $labor_value, '$labor_comment', '$user')")
        c.close()
      case _ =>
    }
  }
  def getSfiCodes: ListBuffer[SfiCode] ={
    val res = ListBuffer.empty[SfiCode]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from sfi order by code")
        while (rs.next()){
          res += new SfiCode(
            rs.getString("code"),
            rs.getString("ru"),
            rs.getString("en"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueSpentTime: ListBuffer[IssueSpentTime] ={
    val res = ListBuffer.empty[IssueSpentTime]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_spent_time")
        while (rs.next()){
          res += IssueSpentTime(
            rs.getInt("issue_id"),
            rs.getLong("labor_date"),
            rs.getDouble("labor_value"),
            rs.getString("labor_comment"),
            rs.getString("user_labor"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setIssueChecks(issue_id: Int, checks: List[IssueCheck]): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from issue_check where issue_id = $issue_id")
        checks.foreach(check => {
          s.execute(s"insert into issue_check (issue_id, check_description, user_login, check_date, check_status, check_group, id, sort) values ($issue_id, '${check.check_description}', '${check.user}', '${check.check_date}', '${check.check_status}', '${check.check_group}', ${check.id}, ${check.sort})")
        })
        s.close()
        c.close()
      case _ =>
    }
  }
  def updateIssueCheck(issue_id: Int, user: String, check_description: String, check_group: String, check_status: Int): Unit ={
    val date = new Date().getTime
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update issue_check set check_status = $check_status, check_date = $date, user_login = '$user' where issue_id = $issue_id and check_description = '$check_description' and check_group = '$check_group'")
        s.close()
        c.close()
      case _ =>
    }
  }
  def getCheckTemplates(user: String): ListBuffer[IssueCheck] ={
    val res = ListBuffer.empty[IssueCheck]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from checklist_templates where author = '$user'")
        while (rs.next()){
          res += new IssueCheck(
            0,
            rs.getString("item"),
            rs.getString("group"),
            user,
            rs.getString("name_template"),
            0,
            0,
            rs.getInt("id"),
            rs.getInt("sort")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }

  def setMessageReaction(jsValue: String): Unit ={
    decode[MessageReaction](jsValue) match {
      case Right(reaction) =>
        GetConnection() match {
          case Some(c) =>
            val s = c.createStatement()
            val date = new Date().getTime
            s.execute(s"insert into message_reactions (message_id, reaction_icon, user_reacted, date_reacted) values (${reaction.message_id}, '${reaction.reaction_icon}', '${reaction.user_reacted}', ${date})")
            s.close()
            c.close()
          case _ =>
        }
      case Left(value) =>
    }
  }
  def deleteMessageReaction(reaction_id: Int): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from message_reactions where id = ${reaction_id}")
        s.close()
        c.close()
      case _ =>
    }
  }
  def getMessageReactions: List[MessageReaction] ={
    val res = ListBuffer.empty[MessageReaction]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from message_reactions")
        while (rs.next()){
          res += MessageReaction(
            rs.getInt("message_id"),
            rs.getString("reaction_icon"),
            rs.getString("user_reacted"),
            rs.getLong("date_reacted")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }

}
