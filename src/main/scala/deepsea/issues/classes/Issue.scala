package deepsea.issues.classes

import deepsea.files.classes.FileAttachment
import play.api.libs.json._

import scala.collection.mutable.ListBuffer

object Issue{
  implicit val writesIssue: Writes[Issue] = new Writes[Issue] {
    override def writes(o: Issue): JsValue = o match {
      case x: Issue => Json.obj(
        "id" -> x.id,
        "status" -> x.status,
        "project" -> x.project,
        "department" -> x.department,
        "started_by" -> x.started_by,
        "started_date" -> x.started_date,
        "issue_type" -> x.issue_type,
        "name" -> x.name,
        "assigned_to" -> x.assigned_to,
        "due_date" -> x.due_date,
        "details" -> x.details,
        "messages" -> x.messages,
        "actions" -> x.actions,
        "file_attachments" -> x.file_attachments,
        "priority" -> x.priority,
        "last_update" -> x.last_update,
        "doc_number" -> x.doc_number,
        "responsible" -> x.responsible,
        "overtime" -> x.overtime,
        "start_date" -> x.start_date,
        "period" -> x.period,
        "child_issues" -> x.child_issues,
        "combined_issues" -> x.combined_issues,
        "file_cloud" -> x.file_cloud,
        "parent_id" -> x.parent_id,
        "history" -> x.history,
        "action" -> x.action,
        "closing_status" -> x.closing_status,

        "issue_comment" -> x.issue_comment,
        "author_comment" -> x.author_comment,
        "first_send_date" -> x.first_send_date,
        "delivered_date" -> x.delivered_date,
        "revision" -> x.revision,
        "revision_files" -> x.revision_files,
        "cloud_files" -> x.cloud_files,
        "archive_revision_files" -> x.archive_revision_files,
        "labor" -> x.labor,
        "checks" -> x.checks,
        "ready" -> x.ready,
        "contract_due_date" -> x.contract_due_date,
        "subscribers" -> x.subscribers,
        "plan_hours" -> x.plan_hours,
        "plan_hours_locked" -> x.plan_hours_locked,
        "assistant" -> x.assistant,
        "reason_of_changes" -> x.reason_of_changes,
        "modification_of_existing" -> x.modification_of_existing,
        "modification_description" -> x.modification_description,
        "contract" -> x.contract,
        "it_type" -> x.it_type,
        "actual_man_hours" -> x.actual_man_hours
      )
      case _ => JsNull
    }
  }
  implicit val readsIssue: Reads[Issue] = new Reads[Issue] {
    override def reads (json: JsValue): JsResult[Issue] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess (new Issue(
        id = (x \ "id").asOpt[Int].getOrElse(0),
        status = (x \ "status").asOpt[String].getOrElse(""),
        project = (x \ "project").asOpt[String].getOrElse(""),
        department = (x \ "department").asOpt[String].getOrElse(""),
        started_by = (x \ "started_by").asOpt[String].getOrElse(""),
        started_date = (x \ "started_date").asOpt[Long].getOrElse(0),
        issue_type = (x \ "issue_type").asOpt[String].getOrElse(""),
        name = (x \ "name").asOpt[String].getOrElse(""),
        details = (x \ "details").asOpt[String].getOrElse(""),
        assigned_to = (x \ "assigned_to").asOpt[String].getOrElse(""),
        due_date = (x \ "due_date").asOpt[Long].getOrElse(0),
        it_type = (x \ "it_type").asOpt[String].getOrElse(""),
      ){
        priority = (x \ "priority").asOpt[String].getOrElse("")
        last_update = (x \ "last_update").asOpt[Long].getOrElse(0)
        doc_number = (x \ "doc_number").asOpt[String].getOrElse("")
        responsible = (x \ "responsible").asOpt[String].getOrElse("")
        overtime = (x \ "overtime").asOpt[String].getOrElse("")
        start_date = (x \ "start_date").asOpt[Long].getOrElse(0)
        period = (x \ "period").asOpt[String].getOrElse("")
        parent_id = (x \ "parent_id").asOpt[Int].getOrElse(0)
        action = (x \ "action").asOpt[String].getOrElse("")
        file_attachments = (x \ "file_attachments").asOpt[ListBuffer[FileAttachment]].getOrElse(ListBuffer.empty[FileAttachment])

        author_comment = (x \ "author_comment").asOpt[String].getOrElse("")
        issue_comment = (x \ "issue_comment").asOpt[String].getOrElse("")
        first_send_date = (x \ "first_send_date").asOpt[Long].getOrElse(0)
        delivered_date = (x \ "delivered_date").asOpt[Long].getOrElse(0)
        revision = (x \ "revision").asOpt[String].getOrElse("")
        ready = (x \ "ready").asOpt[String].getOrElse("000")
        contract_due_date = (x \ "contract_due_date").asOpt[Long].getOrElse(0)
        plan_hours = (x \ "plan_hours").asOpt[Double].getOrElse(0)
        plan_hours_locked = (x \ "plan_hours_locked").asOpt[Int].getOrElse(0)
        assistant = (x \ "assistant").asOpt[String].getOrElse("")
        reason_of_changes = (x \ "reason_of_changes").asOpt[String].getOrElse("")
        modification_of_existing = (x \ "modification_of_existing").asOpt[Int].getOrElse(0)
        modification_description = (x \ "modification_description").asOpt[String].getOrElse("")
        contract = (x \ "contract").asOpt[String].getOrElse("")
      })
      case _ => JsSuccess (null)
    }
  }
}
class Issue(var id: Int, var status: String, var project: String, var department: String, var started_by: String,
            var started_date: Long, var issue_type: String, var name: String, var details: String, var assigned_to: String,
            var due_date: Long, var it_type: String) {
  var action: String = ""
  var actions: ListBuffer[IssueAction] = ListBuffer.empty[IssueAction]
  var file_attachments: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]
  var priority: String = ""
  var last_update: Long = 0
  var doc_number: String = ""
  var responsible: String = ""
  var overtime: String = ""
  var start_date: Long = 0
  var period: String = ""
  var messages: ListBuffer[IssueMessage] = ListBuffer.empty[IssueMessage]
  var child_issues: ListBuffer[ChildIssue] = ListBuffer.empty[ChildIssue]
  var combined_issues: ListBuffer[ChildIssue] = ListBuffer.empty[ChildIssue]
  var file_cloud: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]
  var parent_id: Int = 0
  var history: ListBuffer[IssueHistory] = ListBuffer.empty[IssueHistory]
  var closing_status: String = ""
  var issue_comment: String = ""
  var author_comment: String = ""
  var first_send_date: Long = 0
  var delivered_date: Long = 0
  var revision: String = ""
  var revision_files: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]
  var cloud_files: List[FileAttachment] = List.empty[FileAttachment]
  var archive_revision_files: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]
  var labor: Double = 0
  var checks: ListBuffer[IssueCheck] = ListBuffer.empty[IssueCheck]
  var ready: String = "000"
  var for_revision: String = ""
  var contract_due_date: Long = 0
  var subscribers: List[String] = List.empty[String]
  var plan_hours: Double = 0
  var plan_hours_locked: Int = 0
  var assistant: String = ""
  var reason_of_changes: String = ""
  var modification_of_existing: Int = 0
  var modification_description: String = ""
  var contract: String = ""
  var actual_man_hours: Double = 0
//  var it_type: String = ""
  def toChildIssue: ChildIssue ={
    new ChildIssue(id, status, started_by, started_date, issue_type, name, assigned_to, responsible, doc_number, closing_status)
  }
}
