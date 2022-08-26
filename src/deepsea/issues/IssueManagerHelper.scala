package deepsea.issues

import deepsea.database.DatabaseManager.GetConnection
import deepsea.files.classes.FileAttachment
import deepsea.issues.classes.{Issue, IssueAction}

import scala.collection.mutable.ListBuffer

trait IssueManagerHelper {
  def getRevisionFiles(id: Int): ListBuffer[FileAttachment] ={
    val res = ListBuffer.empty[FileAttachment]
    GetConnection() match {
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
  def getIssueDetails(id: Int): Option[Issue] ={
    var issue: Option[Issue] = Option.empty[Issue]
    GetConnection() match {
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
  def getIssueActions(action: String, issue_type: String): ListBuffer[IssueAction] ={
    val res = ListBuffer.empty[IssueAction]
    GetConnection() match {
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
}
