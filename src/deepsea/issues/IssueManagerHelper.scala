package deepsea.issues

import deepsea.auth.AuthManager.User
import deepsea.database.{DBManager, DatabaseManager, MongoCodecs}
import deepsea.database.DatabaseManager.GetConnection
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManager.DailyTask
import deepsea.issues.classes.{ChildIssue, Issue, IssueAction, IssueCheck, IssueHistory, IssueMessage, IssuePeriod}
import deepsea.time.TimeControlManager.UserWatch
import io.circe.parser.decode
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.Filters.{and, equal}

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait IssueManagerHelper extends MongoCodecs{

  def getIssuesForUser(user: User): ListBuffer[Issue] ={
    val issues = ListBuffer.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        var query = s"select *, (select closing_status from issue_types where type_name = i.issue_type) from issue i where removed = 0 and (assigned_to = '${user.login}' or started_by = '${user.login}' or responsible = '${user.login}'"
        if (user.permissions.contains("view_department_tasks")){
          var groups = user.groups.map(x => "'" + x + "'").mkString("(", ",", ")")
          if (user.groups.isEmpty){
            groups = "('')"
          }
          query += s" or i.issue_type in ${groups}"
        }
        if (user.permissions.contains("view_all_tasks")){
          query += s" or true"
        }
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
            for_revision = rs.getString("for_revision") match {
              case value: String => value
              case _ => "-"
            }
            contract_due_date = rs.getLong("contract_due_date") match {
              case value: Long => value
              case _ => 0
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
        val query = s"select * , (select closing_status from issue_types where type_name = i.issue_type) from issue i where $id=id and removed = 0"
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
            first_local_approval_date = rs.getLong("first_local_approval_date") match {
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
            for_revision = rs.getString("for_revision") match {
              case value: String => value
              case _ => "-"
            }
            contract_due_date = rs.getLong("contract_due_date") match {
              case value: Long => value
              case _ => 0
            }
            revision_files = getRevisionFiles(id)
            archive_revision_files = getRemovedRevisionFiles(id)
            labor = getIssueLabor(id)
            checks = getIssueChecks(id)
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
        var query = s"select * from issue where removed = 0 and parent_id = $id"
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
  def getCombinedIssues(id: Int): ListBuffer[ChildIssue] ={
    val issues = ListBuffer.empty[Issue]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from issue where removed = 0 and id in (select id from issue_combined where issue_first = $id or issue_second = $id) and id != $id"
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
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select stage_name, stage_date, (select p.name as stage_project from issue_projects p where s.stage_project = p.id) from issue_stages s"
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
    GetConnection() match {
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
        DatabaseManager.GetMongoConnection() match {
          case Some(mongo) =>
            val dailyTasks: MongoCollection[DailyTask] = mongo.getCollection("dailyTasks")
            Await.result(dailyTasks.insertOne(value).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  def deleteDailyTask(id: String): Unit ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        val dailyTasks: MongoCollection[DailyTask] = mongo.getCollection("dailyTasks")
        Await.result(dailyTasks.deleteOne(equal("id", id)).toFuture(), Duration(30, SECONDS))
      case _ =>
    }
  }
  def getDailyTasks: List[DailyTask] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        val dailyTasks: MongoCollection[DailyTask] = mongo.getCollection("dailyTasks")
        Await.result(dailyTasks.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[DailyTask] =>
            values.toList
          case _ => List.empty[DailyTask]
        }
      case _ => List.empty[DailyTask]
    }
  }
}
