package deepsea.issues.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, OWrites, Reads, Writes}


object IssueCheck{
  implicit val reads: Reads[IssueCheck] = new Reads[IssueCheck] {
    override def reads(json: JsValue): JsResult[IssueCheck] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueCheck(
        issue_id = (x \ "issue_id").asOpt[Int].getOrElse(0),
        check_description = (x \ "check_description").asOpt[String].getOrElse(""),
        check_group = (x \ "check_group").asOpt[String].getOrElse(""),
        user = (x \ "user").asOpt[String].getOrElse(""),
        template = (x \ "template").asOpt[String].getOrElse(""),
        check_date = (x \ "check_date").asOpt[Long].getOrElse(0),
        check_status = (x \ "check_status").asOpt[Int].getOrElse(0),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[IssueCheck] = new Writes[IssueCheck] {
    override def writes(o: IssueCheck): JsValue = o match {
      case x: IssueCheck => Json.obj(
        "issue_id" -> x.issue_id,
        "check_description" -> x.check_description,
        "check_group" -> x.check_group,
        "user" -> x.user,
        "template" -> x.template,
        "check_date" -> x.check_date,
        "check_status" -> x.check_status,
      )
      case _ => JsNull
    }
  }
}
class IssueCheck(val issue_id: Int, val check_description: String, val check_group: String, val user: String, val template: String, val check_date: Long, val check_status: Int) {

}
