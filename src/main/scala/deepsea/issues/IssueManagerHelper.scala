package deepsea.issues

import deepsea.App
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.User
import deepsea.auth.AuthManagerHelper
import deepsea.database.DBManager.RsIterator
import deepsea.database.{DBManager, DatabaseManager, MongoCodecs}
import deepsea.files.FileManager.{CloudFile, DocumentDirectories}
import deepsea.files.FileManagerHelper
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManager.{DailyTask, GroupFolder, IssueProject, Subscriber}
import deepsea.issues.classes.{ChildIssue, Issue, IssueAction, IssueCheck, IssueHistory, IssueMessage, IssuePeriod, SfiCode}
import deepsea.mail.MailManager
import deepsea.mail.MailManager.Mail
import deepsea.materials.MaterialManager.ProjectName
import deepsea.rocket.RocketChatManager.{SendMessage, SendNotification}
import deepsea.time.PlanHoursHelper
import deepsea.time.TimeControlManager.UserWatch
import io.circe.parser.{decode, parse}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.{and, equal}

import java.nio.file.Files
import java.util.Date
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait IssueManagerHelper extends MongoCodecs {

  def getIssuesForUser(user: User): ListBuffer[Issue] ={
    val issues = ListBuffer.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        //var query = s"select *, (select closing_status from issue_types where type_name = i.issue_type) from issue i where removed = 0"
        var query = Source.fromResource("queries/issues.sql").mkString
        val filterUsers = s" and (assigned_to = '${user.login}' or started_by = '${user.login}' or responsible = '${user.login}'"
        query += filterUsers
//        if (user.permissions.contains("view_department_tasks")){
//          var groups = user.groups.map(x => "'" + x + "'").mkString("(", ",", ")")
//          if (user.groups.isEmpty){
//            groups = "('')"
//          }
//          query += s" or i.issue_type in ${groups}"
//        }
        if (user.permissions.contains("view_all_tasks") || user.login == "op"){
          query += s" or (id > -100)"
        }
        query += s" or (i.department in (select name from issue_departments id where id.manager like '%${user.login}%'))"
        query += s" or (i.project in (select name from issue_projects ip where ip.managers like '%${user.login}%'))"
        query += ")"
        val rs = s.executeQuery(query)
        while (rs.next()){
          issues += new Issue(
            rs.getInt("id") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("status") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("project") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("department") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("started_by") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("started_date") match {
              case value: Long => value
              case _ => 0
            },
            rs.getString("issue_type") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("issue_name") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("details") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("assigned_to") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("due_date") match {
              case value: Long => value
              case _ => 0
            },
          ){
            action =  rs.getString("active_action") match {
              case value: String => value
              case _ => ""
            }
            priority =  rs.getString("priority") match {
              case value: String => value
              case _ => ""
            }
            last_update = rs.getLong("last_update") match {
              case value: Long => value
              case _ => 0
            }
            doc_number =  rs.getString("doc_number") match {
              case value: String => value
              case _ => ""
            }
            responsible =  rs.getString("responsible") match {
              case value: String => value
              case _ => ""
            }
            overtime =  rs.getString("overtime") match {
              case value: String => value
              case _ => ""
            }
            start_date = rs.getLong("start_date") match {
              case value: Long => value
              case _ => 0
            }
            period = rs.getString("period") match {
              case value: String => value
              case _ => ""
            }
            parent_id = rs.getInt("parent_id") match {
              case value: Int => value
              case _ => 0
            }
            closing_status = rs.getString("closing_status") match {
              case value: String => value
              case _ => ""
            }
            delivered_date = rs.getLong("delivered_date") match {
              case value: Long => value
              case _ => 0
            }
            first_send_date = rs.getLong("first_send_date") match {
              case value: Long => value
              case _ => 0
            }
            revision = rs.getString("revision") match {
              case value: String => value
              case _ => ""
            }
            issue_comment = rs.getString("issue_comment") match {
              case value: String => value
              case _ => ""
            }
            ready = rs.getString("ready") match {
              case value: String => value
              case _ => ""
            }
            contract_due_date = rs.getLong("stage_date") match {
              case value: Long => value
              case _ => rs.getLong("contract_due_date") match {
                case value: Long => value
                case _ => 0
              }
            }
            plan_hours = rs.getDouble("plan_hours") match {
              case value: Double => value
              case _ => 0
            }
            plan_hours_locked = rs.getInt("plan_hours_locked") match {
              case value: Int => value
              case _ => 0
            }
            assistant = rs.getString("assistant") match {
              case value: String => value
              case _ => ""
            }
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    issues
  }
  def getQuestions: ListBuffer[Issue] ={
    val issues = ListBuffer.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/issues.sql").mkString
        val filterUsers = s" and issue_type = 'QNA'"
        //val query = s"select * from issue where removed = 0 and issue_type = 'QNA'"
        //val query = s"select * from issue, issue_stages ist where removed = 0 and issue_type = 'QNA' and ist.stage_name = i.period and ist.id_project = (select id from issue_projects ip where ip.name = i.project)";
        val rs = s.executeQuery(query + filterUsers)
        while (rs.next()){
          issues += new Issue(
            rs.getInt("id") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("status") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("project") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("department") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("started_by") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("started_date") match {
              case value: Long => value
              case _ => 0
            },
            rs.getString("issue_type") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("issue_name") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("details") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("assigned_to") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("due_date") match {
              case value: Long => value
              case _ => 0
            },
          ){
            action =  rs.getString("active_action") match {
              case value: String => value
              case _ => ""
            }
            priority =  rs.getString("priority") match {
              case value: String => value
              case _ => ""
            }
            last_update = rs.getLong("last_update") match {
              case value: Long => value
              case _ => 0
            }
            doc_number =  rs.getString("doc_number") match {
              case value: String => value
              case _ => ""
            }
            responsible =  rs.getString("responsible") match {
              case value: String => value
              case _ => ""
            }
            overtime =  rs.getString("overtime") match {
              case value: String => value
              case _ => ""
            }
            start_date = rs.getLong("start_date") match {
              case value: Long => value
              case _ => 0
            }
            period = rs.getString("period") match {
              case value: String => value
              case _ => ""
            }
            parent_id = rs.getInt("parent_id") match {
              case value: Int => value
              case _ => 0
            }
            delivered_date = rs.getLong("delivered_date") match {
              case value: Long => value
              case _ => 0
            }
            first_send_date = rs.getLong("first_send_date") match {
              case value: Long => value
              case _ => 0
            }
            revision = rs.getString("revision") match {
              case value: String => value
              case _ => ""
            }
            issue_comment = rs.getString("issue_comment") match {
              case value: String => value
              case _ => ""
            }
            ready = rs.getString("ready") match {
              case value: String => value
              case _ => ""
            }
            contract_due_date = rs.getLong("contract_due_date") match {
              case value: Long => value
              case _ => 0
            }
            closing_status = rs.getString("closing_status") match {
              case value: String => value
              case _ => ""
            }
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    issues
  }

  def getIssueDetails(id: Int): Option[Issue] ={
    var issue: Option[Issue] = Option.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        //val query = s"select *, (select closing_status from issue_types where type_name = i.issue_type) from issue i where $id = id and removed = 0"
        var query = Source.fromResource("queries/issues.sql").mkString
        query += s" and id = $id"
        val rs = s.executeQuery(query)
        while (rs.next()){
          issue = Option(new Issue(
            rs.getInt("id") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("status") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("project") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("department") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("started_by") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("started_date") match {
              case value: Long => value
              case _ => 0
            },
            rs.getString("issue_type") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("issue_name") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("details") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("assigned_to") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("due_date") match {
              case value: Long => value
              case _ => 0
            },
          ){
            action =  rs.getString("active_action") match {
              case value: String => value
              case _ => ""
            }
            actions = getIssueActions(action, issue_type)
            priority =  rs.getString("priority") match {
              case value: String => value
              case _ => ""
            }
            last_update = rs.getLong("last_update") match {
              case value: Long => value
              case _ => 0
            }
            doc_number =  rs.getString("doc_number") match {
              case value: String => value
              case _ => ""
            }
            responsible =  rs.getString("responsible") match {
              case value: String => value
              case _ => ""
            }
            overtime =  rs.getString("overtime") match {
              case value: String => value
              case _ => ""
            }
            start_date = rs.getLong("start_date") match {
              case value: Long => value
              case _ => 0
            }
            period = rs.getString("period") match {
              case value: String => value
              case _ => ""
            }
            messages = getIssueMessages(id)
            file_attachments = getIssueFileAttachments(id)
            parent_id = rs.getInt("parent_id") match {
              case value: Int => value
              case _ => 0
            }
            history = getIssueHistory(id)
            child_issues = getChildIssues(id)
            combined_issues = getCombinedIssues(id)
            closing_status = rs.getString("closing_status") match {
              case value: String => value
              case _ => ""
            }
            delivered_date = rs.getLong("delivered_date") match {
              case value: Long => value
              case _ => 0
            }
            first_send_date = rs.getLong("first_send_date") match {
              case value: Long => value
              case _ => 0
            }
            revision = rs.getString("revision") match {
              case value: String => value
              case _ => ""
            }
            issue_comment = rs.getString("issue_comment") match {
              case value: String => value
              case _ => ""
            }
            ready = rs.getString("ready") match {
              case value: String => value
              case _ => ""
            }
            contract_due_date = rs.getLong("stage_date") match {
              case value: Long => value
              case _ => rs.getLong("contract_due_date") match {
                case value: Long => value
                case _ => 0
              }
            }
            plan_hours = rs.getDouble("plan_hours") match {
              case value: Double => value
              case _ => 0
            }
            plan_hours_locked = rs.getInt("plan_hours_locked") match {
              case value: Int => value
              case _ => 0
            }
            assistant = rs.getString("assistant") match {
              case value: String => value
              case _ => ""
            }
            revision_files = getRevisionFiles(id)
            cloud_files = if (issue_type == "RKD") getCloudFiles(project, doc_number, department) else List.empty[FileAttachment]
            //cloud_files = List.empty[FileAttachment]
            archive_revision_files = getRemovedRevisionFiles(id)
            labor = getIssueLabor(id)
            checks = getIssueChecks(id)
            subscribers = getIssueSubscribers(id).map(_.user)
            reason_of_changes = Option(rs.getString("reason_of_changes")).getOrElse("")
            modification_of_existing = Option(rs.getInt("modification_of_existing")).getOrElse(0)
            modification_description = Option(rs.getString("modification_description")).getOrElse("")
          })
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    issue
  }
  def getIssueDetails(docNumber: String): Option[Issue] ={
    var issue: Option[Issue] = Option.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        //val query = s"select *, (select closing_status from issue_types where type_name = i.issue_type) from issue i where $id = id and removed = 0"
        var query = Source.fromResource("queries/issues.sql").mkString
        query += s" and doc_number = '$docNumber'"
        val rs = s.executeQuery(query)
        while (rs.next()){
          issue = Option(new Issue(
            rs.getInt("id") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("status") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("project") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("department") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("started_by") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("started_date") match {
              case value: Long => value
              case _ => 0
            },
            rs.getString("issue_type") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("issue_name") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("details") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("assigned_to") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("due_date") match {
              case value: Long => value
              case _ => 0
            },
          ){
            action =  rs.getString("active_action") match {
              case value: String => value
              case _ => ""
            }
            actions = getIssueActions(action, issue_type)
            priority =  rs.getString("priority") match {
              case value: String => value
              case _ => ""
            }
            last_update = rs.getLong("last_update") match {
              case value: Long => value
              case _ => 0
            }
            doc_number =  rs.getString("doc_number") match {
              case value: String => value
              case _ => ""
            }
            responsible =  rs.getString("responsible") match {
              case value: String => value
              case _ => ""
            }
            overtime =  rs.getString("overtime") match {
              case value: String => value
              case _ => ""
            }
            start_date = rs.getLong("start_date") match {
              case value: Long => value
              case _ => 0
            }
            period = rs.getString("period") match {
              case value: String => value
              case _ => ""
            }
            messages = getIssueMessages(id)
            file_attachments = getIssueFileAttachments(id)
            parent_id = rs.getInt("parent_id") match {
              case value: Int => value
              case _ => 0
            }
            history = getIssueHistory(id)
            child_issues = getChildIssues(id)
            combined_issues = getCombinedIssues(id)
            closing_status = rs.getString("closing_status") match {
              case value: String => value
              case _ => ""
            }
            delivered_date = rs.getLong("delivered_date") match {
              case value: Long => value
              case _ => 0
            }
            first_send_date = rs.getLong("first_send_date") match {
              case value: Long => value
              case _ => 0
            }
            revision = rs.getString("revision") match {
              case value: String => value
              case _ => ""
            }
            issue_comment = rs.getString("issue_comment") match {
              case value: String => value
              case _ => ""
            }
            ready = rs.getString("ready") match {
              case value: String => value
              case _ => ""
            }
            contract_due_date = rs.getLong("stage_date") match {
              case value: Long => value
              case _ => rs.getLong("contract_due_date") match {
                case value: Long => value
                case _ => 0
              }
            }
            plan_hours = rs.getDouble("plan_hours") match {
              case value: Double => value
              case _ => 0
            }
            plan_hours_locked = rs.getInt("plan_hours_locked") match {
              case value: Int => value
              case _ => 0
            }
            assistant = rs.getString("assistant") match {
              case value: String => value
              case _ => ""
            }
            revision_files = getRevisionFiles(id)
            cloud_files = if (issue_type == "RKD") getCloudFiles(project, doc_number, department) else List.empty[FileAttachment]
            //cloud_files = List.empty[FileAttachment]
            archive_revision_files = getRemovedRevisionFiles(id)
            labor = getIssueLabor(id)
            checks = getIssueChecks(id)
            subscribers = getIssueSubscribers(id).map(_.user)
            reason_of_changes = Option(rs.getString("reason_of_changes")).getOrElse("")
            modification_of_existing = Option(rs.getInt("modification_of_existing")).getOrElse(0)
            modification_description = Option(rs.getString("modification_description")).getOrElse("")
          })
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    issue
  }

  def getIssueLabor(issue_id: Int): Double ={
    var res: Double = 0
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select sum(labor_value) as issue_labor from issue_spent_time where issue_id = $issue_id")
        while (rs.next()){
          res = rs.getDouble("issue_labor")
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueChecks(issue_id: Int): ListBuffer[IssueCheck] ={
    val res = ListBuffer.empty[IssueCheck]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_check where issue_id = $issue_id")
        while (rs.next()){
          res += new IssueCheck(
            rs.getInt("issue_id"),
            rs.getString("check_description"),
            rs.getString("check_group"),
            rs.getString("user_login"),
            "No Template",
            rs.getLong("check_date"),
            rs.getInt("check_status"),
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
  def getRemovedRevisionFiles(id: Int): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from revision_files where issue_id = $id and removed = 1")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("file_name"),
            rs.getString("file_url"),
            rs.getLong("upload_date"),
            rs.getString("upload_user"),
            rs.getString("issue_revision"),
            rs.getString("group_name"),
          ){
            removed_date = rs.getLong("removed_date")
            removed_by = rs.getString("removed_by")
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getRevisionFiles(id: Int): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from revision_files where issue_id = $id and removed = 0")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("file_name"),
            rs.getString("file_url"),
            rs.getLong("upload_date"),
            rs.getString("upload_user"),
            rs.getString("issue_revision"),
            rs.getString("group_name"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getCombinedIssuesAux(issue_id: Int): ListBuffer[Int] ={
    val combined = ListBuffer.empty[Int]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_combined where issue_first = $issue_id or issue_second = $issue_id")
        while (rs.next()){
          List(Option(rs.getInt("issue_first")).getOrElse(0), Option(rs.getInt("issue_second")).getOrElse(0)).foreach(issue => {
            if (!(combined ++ List(issue_id)).contains(issue)){
              combined += issue
            }
          })
        }
        s.close()
        c.close()
      case _ =>
    }
    combined
  }
  def getChildIssues(id: Int): ListBuffer[ChildIssue] ={
    val issues = ListBuffer.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select *, (select closing_status from issue_types where type_name = i.issue_type) from issue i where removed = 0 and parent_id = $id"
        val rs = s.executeQuery(query)
        while (rs.next()){
          issues += new Issue(
            rs.getInt("id") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("status") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("project") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("department") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("started_by") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("started_date") match {
              case value: Long => value
              case _ => 0
            },
            rs.getString("issue_type") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("issue_name") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("details") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("assigned_to") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("due_date") match {
              case value: Long => value
              case _ => 0
            },
          ){
            action =  rs.getString("active_action") match {
              case value: String => value
              case _ => ""
            }
            priority =  rs.getString("priority") match {
              case value: String => value
              case _ => ""
            }
            last_update = rs.getLong("last_update") match {
              case value: Long => value
              case _ => 0
            }
            doc_number =  rs.getString("doc_number") match {
              case value: String => value
              case _ => ""
            }
            responsible =  rs.getString("responsible") match {
              case value: String => value
              case _ => ""
            }
            overtime =  rs.getString("overtime") match {
              case value: String => value
              case _ => ""
            }
            start_date = rs.getLong("start_date") match {
              case value: Long => value
              case _ => 0
            }
            period = rs.getString("period") match {
              case value: String => value
              case _ => ""
            }
            parent_id = rs.getInt("parent_id") match {
              case value: Int => value
              case _ => 0
            }
            delivered_date = rs.getLong("delivered_date") match {
              case value: Long => value
              case _ => 0
            }
            first_send_date = rs.getLong("first_send_date") match {
              case value: Long => value
              case _ => 0
            }
            revision = rs.getString("revision") match {
              case value: String => value
              case _ => ""
            }
            issue_comment = rs.getString("issue_comment") match {
              case value: String => value
              case _ => ""
            }
            closing_status = rs.getString("closing_status") match {
              case value: String => value
              case _ => ""
            }
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    issues.map(x => x.toChildIssue)
  }
  def getCombinedIssues(id: Int): ListBuffer[ChildIssue] ={
    val issues = ListBuffer.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from issue where removed = 0 and (id in (select issue_first from issue_combined where issue_first = $id or issue_second = $id) or id in (select issue_second from issue_combined where issue_first = $id or issue_second = $id)) and id != $id"
        val rs = s.executeQuery(query)
        while (rs.next()){
          issues += new Issue(
            rs.getInt("id") match {
              case value: Int => value
              case _ => 0
            },
            rs.getString("status") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("project") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("department") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("started_by") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("started_date") match {
              case value: Long => value
              case _ => 0
            },
            rs.getString("issue_type") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("issue_name") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("details") match {
              case value: String => value
              case _ => ""
            },
            rs.getString("assigned_to") match {
              case value: String => value
              case _ => ""
            },
            rs.getLong("due_date") match {
              case value: Long => value
              case _ => 0
            },
          ){
            action =  rs.getString("active_action") match {
              case value: String => value
              case _ => ""
            }
            priority =  rs.getString("priority") match {
              case value: String => value
              case _ => ""
            }
            last_update = rs.getLong("last_update") match {
              case value: Long => value
              case _ => 0
            }
            doc_number =  rs.getString("doc_number") match {
              case value: String => value
              case _ => ""
            }
            responsible =  rs.getString("responsible") match {
              case value: String => value
              case _ => ""
            }
            overtime =  rs.getString("overtime") match {
              case value: String => value
              case _ => ""
            }
            start_date = rs.getLong("start_date") match {
              case value: Long => value
              case _ => 0
            }
            period = rs.getString("period") match {
              case value: String => value
              case _ => ""
            }
            parent_id = rs.getInt("parent_id") match {
              case value: Int => value
              case _ => 0
            }
            delivered_date = rs.getLong("delivered_date") match {
              case value: Long => value
              case _ => 0
            }
            first_send_date = rs.getLong("first_send_date") match {
              case value: Long => value
              case _ => 0
            }
            revision = rs.getString("revision") match {
              case value: String => value
              case _ => ""
            }
            issue_comment = rs.getString("issue_comment") match {
              case value: String => value
              case _ => ""
            }
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    issues.map(x => x.toChildIssue)
  }

  def getIssuePeriods: ListBuffer[IssuePeriod] ={
    val res = ListBuffer.empty[IssuePeriod]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select stage_name, stage_date, (select p.name as stage_project from issue_projects p where s.id_project = p.id) from issue_stages s"
        val rs = s.executeQuery(query)
        while (rs.next()){
          res += new IssuePeriod(rs.getString("stage_name"), rs.getString("stage_project"), rs.getLong("stage_date"))
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res
  }
  def getIssueActions(action: String, issue_type: String): ListBuffer[IssueAction] ={
    val res = ListBuffer.empty[IssueAction]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_action where current_action = '$action' and issue_type = '$issue_type'")
        while (rs.next()){
          val actions = rs.getString("available_actions").split(",").map(_.trim).filter(_ != "")
          val rules = rs.getString("rules").split(",").map(_.trim).filter(_ != "").mkString
          actions.foreach(a => {
            res += new IssueAction(a, rules)
          })
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueHistory(id: Int): ListBuffer[IssueHistory] ={
    val res = ListBuffer.empty[IssueHistory]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_history where id = $id")
        while (rs.next()){
          res += new IssueHistory(rs.getInt("id"), rs.getString("author"), rs.getString("name_value"),
            rs.getString("prev_value"), rs.getString("new_value"), rs.getLong("update_date"), rs.getString("update_message"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueMessages(id: Int): ListBuffer[IssueMessage] ={
    val res = ListBuffer.empty[IssueMessage]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_messages where issue_id = $id and removed = 0")
        while (rs.next()){
          res += new IssueMessage(
            rs.getString("author"),
            rs.getString("content"),
            rs.getLong("date"),
            rs.getString("prefix"),
            rs.getInt("to_be_replied"),
          ){
            file_attachments = getMessageFileAttachments(rs.getInt("id"))
          }
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getIssueFileAttachments(id: Int): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from file_attachments where issue_id = '$id'")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("file_name"),
            rs.getString("url"),
            rs.getLong("upload_date"),
            rs.getString("author"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getMessageFileAttachments(id: Int): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from file_attachments where message_id = $id")
        while (rs.next()){
          res += new FileAttachment(
            rs.getString("file_name"),
            rs.getString("url"),
            rs.getLong("upload_date"),
            rs.getString("author"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }


  def addDailyTask(json: String): Unit ={
    decode[DailyTask](json) match {
      case Right(value) =>
        setIssueLabor(value.issueId, value.date, value.time, value.details, value.userLogin, value.dateCreated, value.id, value.project)
      case _ => None
    }
  }
  def deleteDailyTask(id: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from issue_spent_time where id = $id")
        s.close()
        c.close()
      case _ =>
    }
  }
  def setIssueLabor(issue_id: Int, labor_date: Long, labor_value: Double, labor_comment: String, user_labor: String, date_created: Long, id: Int, project: String): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into issue_spent_time (issue_id, labor_date, labor_value, labor_comment, user_labor, date_created, id, project) values ($issue_id, $labor_date, $labor_value, '$labor_comment', '$user_labor', $date_created, default, '$project')")
        s.close()
        c.close()
      case _ =>
    }
  }
  def setIssuePeriods(issue_id: Int, start_date: Long, end_date: Long): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update issue set start_date = $start_date, due_date = $end_date where id = $issue_id")
        s.close()
        c.close()
      case _ =>
    }
  }
  def getDailyTasks: List[DailyTask] ={
    getIssueSpentTime.toList
  }
  def getIssueSpentTime: ListBuffer[DailyTask] ={
    val res = ListBuffer.empty[DailyTask]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_spent_time")
        while (rs.next()){
          res += DailyTask(
            rs.getInt("issue_id"),
            rs.getLong("labor_date"),
            rs.getLong("date_created"),
            rs.getString("user_labor"),
            rs.getString("project"),
            rs.getString("labor_comment"),
            rs.getDouble("labor_value"),
            rs.getInt("id")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
//  def getDailyTasks: List[DailyTask] ={
//    DBManager.GetMongoConnection() match {
//      case Some(mongo) =>
//        val dailyTasks: MongoCollection[DailyTask] = mongo.getCollection("dailyTasks")
//        Await.result(dailyTasks.find().toFuture(), Duration(30, SECONDS)) match {
//          case values: Seq[DailyTask] =>
//            values.toList
//          case _ => List.empty[DailyTask]
//        }
//      case _ => List.empty[DailyTask]
//    }
//  }
  def getCloudFiles(project: String, docNumber: String, department: String): List[FileAttachment]={
    val spCloud: String = "/"
    val res = ListBuffer.empty[FileAttachment]
    getProjectNames.find(_.pdsp == project) match {
      case Some(projectName) =>
        getDocumentDirectories.find(x => x.project == projectName.rkd && x.department == department) match {
          case Some(docDirectories) =>
            val filter = List(projectName.cloudRkd, "Documents", department, docNumber).mkString(spCloud)
            val cloudFiles = getCloudFiles(filter)
            docDirectories.directories.foreach(p => {
              val path = filter + spCloud + p
              cloudFiles.filter(x => x.url.contains(path) && x.name.contains(".")).foreach(cFile => {
                res += new FileAttachment(
                  cFile.name,
                  cFile.url,
                  cFile.upload_date,
                  cFile.author,
                  "",
                  p,
                  1
                )
              })
            })
          case _ =>
        }
      case _ =>
    }
    res.toList
  }
  def getCloudFiles(id: Int): List[FileAttachment]={
    val spCloud: String = "/"
    val res = ListBuffer.empty[FileAttachment]
    getIssueDetails(id) match {
      case Some(issue) =>
        getProjectNames.find(_.pdsp == issue.project) match {
          case Some(projectName) =>
            getDocumentDirectories.find(x => x.project == projectName.rkd && x.department == issue.department) match {
              case Some(docDirectories) =>
                val pathFull = List(projectName.cloudRkd, "Documents", issue.department, issue.doc_number).mkString(spCloud)

                val cloudFiles = getCloudFiles(pathFull)

                docDirectories.directories.foreach(p => {
                  val path = pathFull + spCloud + p
                  cloudFiles.filter(x => x.url.contains(path) && x.name.contains(".")).foreach(cFile => {
                    res += new FileAttachment(
                      cFile.name,
                      cFile.url,
                      cFile.upload_date,
                      cFile.author,
                      "",
                      p,
                      1
                    )
                  })
                })
              case _ =>
            }
          case _ =>
        }
      case _ => None
    }
    res.toList
  }
  def getDocumentDirectories: List[DocumentDirectories] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("document-directories").find[DocumentDirectories]().toFuture(), Duration(30, SECONDS)) match {
          case projectNames => projectNames.toList
          case _ => List.empty[DocumentDirectories]
        }
      case _ => List.empty[DocumentDirectories]
    }
  }
  def getProjectNames: List[ProjectName] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("project-names").find[ProjectName]().toFuture(), Duration(30, SECONDS)) match {
          case projectNames => projectNames.toList
          case _ => List.empty[ProjectName]
        }
      case _ => List.empty[ProjectName]
    }
  }
  def getCloudFiles(filter: String): List[FileAttachment]= {
    val res = ListBuffer.empty[FileAttachment]
    val groupFolders = ListBuffer.empty[GroupFolder]
    DBManager.GetNextCloudConnection() match {
      case Some(cloudConnection) =>
        val stmt = cloudConnection.createStatement()
        var query = s"select * from oc_group_folders"
        var rs = stmt.executeQuery(query)
        while (rs.next()){
          groupFolders += GroupFolder(
            rs.getInt("folder_id"),
            rs.getString("mount_point")
          )
        }
        var searchFilter = filter
        groupFolders.find(x => filter.startsWith(x.name)) match {
          case Some(value) => searchFilter = filter.replace(value.name + "/", "__groupfolders/" + value.id.toString + "/")
          case _ => None
        }

        query = Source.fromResource("queries/cloud-files.sql").mkString.replace("&filter", searchFilter)

        rs = stmt.executeQuery(query)
        while (rs.next()){
          val name = rs.getString("name")
          val path = rs.getString("path")
          res += new FileAttachment(
            name,
            App.HTTPServer.RestUrl + "/" + "cloud/" + name + "?path=" + (groupFolders.find(x => path.startsWith("__groupfolders/" + x.id)) match {
              case Some(value) =>
                path.replace("__groupfolders/" + value.id.toString + "/", value.name + "/")
              case _ => path
            }),
            Option(rs.getLong("mtime")).getOrElse(0),
            Option(rs.getString("user")).getOrElse("op"),
            "",
            "",
            1
          )
        }
        rs.close()
        stmt.close()
        cloudConnection.close()
      case _ => None
    }
    res.toList
  }

  def getCloudFilesAux(filter: String): List[FileAttachment]= {
    val res = ListBuffer.empty[FileAttachment]
    val cloudFiles = DBManager.GetNextCloudConnection() match {
      case Some(cloudConnection) =>
        val stmt = cloudConnection.createStatement()
        val query = s"select * from oc_activity where file like '%$filter%' and subject = 'created_self' and object_id not in (select fileid from oc_filecache where path like '%/trash/%' or path like '%/arh/%') and object_id not in (select object_id from oc_activity where subject = 'deleted_self')"
        //val query = s"select * from oc_filecache where path like '%$filter%' and path not like '%/trash/%'"
        val resultSet = RsIterator(stmt.executeQuery(query))
        val res = resultSet.map(rs => {
          CloudFile(
            rs.getLong("timestamp"),
            rs.getString("type"),
            rs.getString("subject"),
            rs.getString("user"),
            rs.getString("file"),
            rs.getString("link"),
            rs.getInt("object_id")
          )
        }).toList
        resultSet.rs.close()
        stmt.close()
        cloudConnection.close()
        res
      case _ => List.empty[CloudFile]
    }
    val cloudFilesActive = cloudFiles
    cloudFilesActive.foreach(cFile => {
      res += new FileAttachment(
        cFile.file.split("/").last,
        App.HTTPServer.RestUrl + "/" + "cloud/" + cFile.file.split("/").last + "?path=" + cFile.file,
        cFile.timeStamp,
        cFile.user,
        "",
        "",
        1
      )
    })
    res.toList
  }
  def getCloudFilesAux(id: Int): List[FileAttachment]={
    val spCloud: String = "/"
    val res = ListBuffer.empty[FileAttachment]
    getIssueDetails(id) match {
      case Some(issue) =>
        getProjectNames.find(_.pdsp == issue.project) match {
          case Some(projectName) =>
            getDocumentDirectories.find(x => x.project == projectName.rkd && x.department == issue.department) match {
              case Some(docDirectories) =>
                val pathFull = List(projectName.cloud, "Documents", issue.department, issue.doc_number).mkString(spCloud)

                val cloudFiles = DBManager.GetNextCloudConnection() match {
                  case Some(cloudConnection) =>
                    val stmt = cloudConnection.createStatement()
                    val query = s"select * from oc_activity where file like '%$pathFull%'"
                    val resultSet = RsIterator(stmt.executeQuery(query))
                    val res = resultSet.map(rs => {
                      CloudFile(
                        rs.getLong("timestamp"),
                        rs.getString("type"),
                        rs.getString("subject"),
                        rs.getString("user"),
                        rs.getString("file"),
                        rs.getString("link"),
                        rs.getInt("object_id")
                      )
                    }).toList
                    resultSet.rs.close()
                    stmt.close()
                    cloudConnection.close()
                    res
                  case _ => List.empty[CloudFile]
                }

                val cloudFilesActive = cloudFiles.filter(x => x.typeAction == "file_created" && !cloudFiles.exists(y => y.typeAction == "file_deleted" && y.id == x.id))

                docDirectories.directories.foreach(p => {
                  val path = pathFull + spCloud + p
                  cloudFilesActive.filter(x => x.file.contains(path) && x.file.split("/").last.contains(".")).foreach(cFile => {
                    res += new FileAttachment(
                      cFile.file.split("/").last,
                      App.HTTPServer.RestUrl + "/" + "cloud/" + cFile.file.split("/").last + "?path=" + cFile.file,
                      cFile.timeStamp,
                      cFile.user,
                      "",
                      p,
                      1
                    )
                  })
                })
              case _ =>
            }
          case _ =>
        }
      case _ => None
    }
    res.toList
  }
  def getCloudFilesAux(project: String, docNumber: String, department: String): List[FileAttachment]={
    val spCloud: String = "/"
    val res = ListBuffer.empty[FileAttachment]
    getProjectNames.find(_.pdsp == project) match {
      case Some(projectName) =>
        getDocumentDirectories.find(x => x.project == projectName.rkd && x.department == department) match {
          case Some(docDirectories) =>
            val filter = List(projectName.cloudRkd, "Documents", department, docNumber).mkString(spCloud)

            val cloudFiles = DBManager.GetNextCloudConnection() match {
              case Some(cloudConnection) =>
                val stmt = cloudConnection.createStatement()
                val query = s"select * from oc_activity where file like '%$filter%' and subject = 'created_self' and object_id not in (select fileid from oc_filecache where path like '%/trash/%' or path like '%/arh/%') and object_id not in (select object_id from oc_activity where subject = 'deleted_self')"
                val resultSet = RsIterator(stmt.executeQuery(query))
                val res = resultSet.map(rs => {
                  CloudFile(
                    rs.getLong("timestamp"),
                    rs.getString("type"),
                    rs.getString("subject"),
                    rs.getString("user"),
                    rs.getString("file"),
                    rs.getString("link"),
                    rs.getInt("object_id")
                  )
                }).toList
                resultSet.rs.close()
                stmt.close()
                cloudConnection.close()
                res
              case _ => List.empty[CloudFile]
            }

            docDirectories.directories.foreach(p => {
              val path = filter + spCloud + p
              cloudFiles.filter(x => x.file.contains(path) && x.file.split("/").last.contains(".")).foreach(cFile => {
                res += new FileAttachment(
                  cFile.file.split("/").last,
                  App.HTTPServer.RestUrl + "/" + "cloud/" + cFile.file.split("/").last + "?path=" + cFile.file,
                  //                  "http://192.168.1.122:1112" + "/" + "cloud?path=" + cFile.file,
                  cFile.timeStamp,
                  cFile.user,
                  "",
                  p,
                  1
                )
              })
            })
          case _ =>
        }
      case _ =>
    }
    res.toList
  }
  def subscribeForIssueNotifications(user: String, issue: Int, options: String): String ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select count(*) from issue_subscriptions where user_login = '$user' and issue_id = $issue")
        val count = if (rs.next()){
          rs.getInt(1)
        }
        else{
          0
        }
        rs.close()
        if (count > 0){
          s.execute(s"update issue_subscriptions set options = $options where user_login = '$user' and issue_id = $issue")
        }
        else{
          s.execute(s"insert into issue_subscriptions values ('$user', $issue, '$options')")
        }
        s.close()
        c.close()
      case _ =>
    }
    "success"
  }
  def getIssueSubscribers(issue: Int): List[Subscriber] ={
    val res = ListBuffer.empty[Subscriber]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_subscriptions where issue_id = $issue and options != ''")
        while(rs.next()){
          res += Subscriber(rs.getString("user_login"), rs.getString("options"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }
  def notifyDocUpload(taskId: Int): String ={
    getIssueDetails(taskId) match {
      case Some(issue) =>
        val projects = getIssueProjects
        val department = issue.department match {
          case "Hull" => "hull-esp"
          case "System" => "pipe-esp"
          case "Devices" => "device-esp"
          case "Trays" => "trays"
          case "Cables" => "cables"
          case "Electric" => "electric-esp"
          case "Accommodation" => "accommodation-esp"
          case "Design" => "design-esp"
        }
        val project = projects.find(x => x.name == issue.project) match {
          case Some(value) => value.foran
          case _ => issue.project
        }
        val url = App.HTTPServer.Url + "/" + department + "?issueId=" + issue.id + "&foranProject=" + project + "&docNumber=" + issue.doc_number + "&department=" + issue.department + "&nc=1"

        DBManager.GetPGConnection() match {
          case Some(c) =>
            val s = c.createStatement()
            val q = Source.fromResource("queries/userNotificationsByProject.sql").mkString.replace("&project", issue.project)
            val rsIter = RsIterator(s.executeQuery(q))
            val emails = rsIter.map(rs => {
              rs.getString("email")
            })
            //val url = App.HTTPServer.Url + "/?taskId=" + issue.id
            val text = Source.fromResource("messages/newDoc.html").mkString
              .replace("&docNumber", issue.doc_number)
              .replace("&project", issue.project)
              .replace("&name", issue.name)
              .replace("&type", issue.issue_type)
              .replace("&department", issue.department)
              .replace("&revision", issue.revision)
              .replace("&url", url)
            emails.foreach(email => {
              ActorManager.mail ! Mail("Nautic Rus", email, "New documents has been added", text)
            })
            rsIter.rs.close()
            s.close()
            c.close()
          case _ =>
        }
      case _ => None
    }
    "success"
  }
  def updateIssueLabor(issue_id: Int, labor: Double): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update issue set plan_hours = $labor where id = $issue_id")
        s.close()
        c.close()
      case _ =>
    }
  }
  def updateLockPlanHours(issue_id: Int, state: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update issue set plan_hours_locked = $state where id = $issue_id")
        s.close()
        c.close()
      case _ =>
    }
  }
  def getSfiCodes: ListBuffer[SfiCode] ={
    val res = ListBuffer.empty[SfiCode]
    DBManager.GetPGConnection() match {
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
  def getIssueProjects: ListBuffer[IssueProject] ={
    val res = ListBuffer.empty[IssueProject]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_projects where status = 0 order by id")
        while (rs.next()){
          res += IssueProject(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getString("name")).getOrElse(""),
            Option(rs.getString("pdsp")).getOrElse(""),
            Option(rs.getString("rkd")).getOrElse(""),
            Option(rs.getString("foran")).getOrElse(""),
            Option(rs.getString("managers")).getOrElse(""),
            Option(rs.getString("status")).getOrElse(""),
            Option(rs.getString("factory")).getOrElse(""),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }

  def getProjectDetails(id: String): Option[IssueProject] = {
    var project: Option[IssueProject] = Option.empty[IssueProject]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_projects where id = $id")
        while (rs.next()) {
          project = Option(IssueProject(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getString("name")).getOrElse(""),
            Option(rs.getString("pdsp")).getOrElse(""),
            Option(rs.getString("rkd")).getOrElse(""),
            Option(rs.getString("foran")).getOrElse(""),
            Option(rs.getString("managers")).getOrElse(""),
            Option(rs.getString("status")).getOrElse(""),
            Option(rs.getString("factory")).getOrElse(""),
          ))
        }
        rs.close()
        s.close()
        c.close()
        project
      case _ => Option.empty[IssueProject]
    }
  }

  def startProject(project: IssueProject): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"insert into issue_projects (id, name, foran, rkd, pdsp, factory, managers, status) values (default, '${project.name}', '${project.foran}', '${project.pdsp}', '${project.rkd}', '${project.factory}', '${project.managers}', default)";
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }

  def editProject(project: IssueProject, id: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"update issue_projects set name = '${project.name}', foran = '${project.foran}', rkd = '${project.rkd}', pdsp = '${project.pdsp}', factory = '${project.factory}', managers = '${project.managers}', status = '${project.status}' where id = '$id'"
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }

  def deleteProject(id: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"update issue_projects set status = '1' where id = $id";
        s.execute(query);
        val delQuery = s"delete from users_visibility_projects where project_id = '$id'";
        s.execute(delQuery);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }
  def transliterate(message: String): String = {
    val abcCyr: Array[Char] = Array(' ', 'а', 'б', 'в', 'г', 'д', 'е', 'ё', 'ж', 'з', 'и', 'й', 'к', 'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы', 'ь', 'э', 'ю', 'я', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ё', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М', 'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы', 'Ь', 'Э', 'Ю', 'Я', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
    val abcLat: Array[String] = Array(" ", "a", "b", "v", "g", "d", "e", "e", "zh", "z", "i", "y", "k", "l", "m", "n", "o", "p", "r", "s", "t", "u", "f", "h", "ts", "ch", "sh", "sch", "", "i", "", "e", "ju", "ja", "A", "B", "V", "G", "D", "E", "E", "Zh", "Z", "I", "Y", "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "F", "H", "Ts", "Ch", "Sh", "Sch", "", "I", "", "E", "Ju", "Ja", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
    val builder: mutable.StringBuilder = new mutable.StringBuilder
    for (i <- 0 until message.length) {
      for (x <- 0 until abcCyr.length) {
        if (message.charAt(i) == abcCyr(x)) builder.append(abcLat(x))
      }
    }
    builder.toString
  }
}