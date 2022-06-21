package deepsea.issues.classes

import play.api.libs.json._

object ChildIssue{
  implicit val writesIssue: Writes[ChildIssue] = new Writes[ChildIssue] {
    override def writes(o: ChildIssue): JsValue = o match {
      case x: ChildIssue => Json.obj(
        "id" -> x.id,
        "status" -> x.status,
        "started_by" -> x.started_by,
        "started_date" -> x.started_date,
        "issue_type" -> x.issue_type,
        "name" -> x.name,
        "assigned_to" -> x.assigned_to,
        "last_update" -> x.last_update,
        "doc_number" -> x.doc_number,
        "responsible" -> x.responsible,
      )
      case _ => JsNull
    }
  }
  implicit val readsIssue: Reads[ChildIssue] = new Reads[ChildIssue] {
    override def reads (json: JsValue): JsResult[ChildIssue] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess (new ChildIssue(
        id = (x \ "id").asOpt[Int].getOrElse(0),
        status = (x \ "status").asOpt[String].getOrElse(""),
        started_by = (x \ "started_by").asOpt[String].getOrElse(""),
        started_date = (x \ "started_date").asOpt[Long].getOrElse(0),
        issue_type = (x \ "issue_type").asOpt[String].getOrElse(""),
        name = (x \ "name").asOpt[String].getOrElse(""),
        assigned_to = (x \ "assigned_to").asOpt[String].getOrElse(""),
        responsible = (x \ "responsible").asOpt[String].getOrElse("")
      ){
        last_update = (x \ "last_update").asOpt[String].getOrElse("")
        doc_number = (x \ "doc_number").asOpt[String].getOrElse("")
      })
      case _ => JsSuccess (null)
    }
  }
}
class ChildIssue(var id: Int, var status: String, var started_by: String,
            var started_date: Long, var issue_type: String, var name: String, var assigned_to: String, var responsible: String) {
  var last_update: String = ""
  var doc_number: String = ""
}
