package deepsea.issues.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

object IssueAction{
  implicit val reads: Reads[IssueAction] = new Reads[IssueAction] {
    override def reads(json: JsValue): JsResult[IssueAction] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueAction(
        action = (x \ "current_action").asOpt[String].getOrElse(""),
        rule = (x \ "available_actions").asOpt[String].getOrElse(""),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[IssueAction] = new Writes[IssueAction] {
    override def writes(o: IssueAction): JsValue = o match {
      case x: IssueAction => Json.obj(
        "action" -> x.action,
        "rule" -> x.rule,
      )
      case _ => JsNull
    }
  }
}
class IssueAction(val action: String, val rule: String) {

}
