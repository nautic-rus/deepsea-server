package deepsea.issues.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

object IssueType{
  implicit val reads: Reads[IssueType] = new Reads[IssueType] {
    override def reads(json: JsValue): JsResult[IssueType] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueType(
        type_name = (x \ "type_name").asOpt[String].getOrElse(""),
        local_approval = (x \ "local_approval").asOpt[String].getOrElse(""),
        yard_approval = (x \ "yard_approval").asOpt[String].getOrElse(""),
        sort = (x \ "value").asOpt[Int].getOrElse(0),
        visible_row = (x \ "visible_row").asOpt[String].getOrElse(""),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[IssueType] = new Writes[IssueType] {
    override def writes(o: IssueType): JsValue = o match {
      case x: IssueType => Json.obj(
        "type_name" -> x.type_name,
        "local_approval" -> x.local_approval,
        "yard_approval" -> x.yard_approval,
        "sort" -> x.sort,
        "visible_row" -> x.visible_row,
      )
      case _ => JsNull
    }
  }
}
class IssueType(val type_name: String, val local_approval: String, val yard_approval: String, val sort: Int, val visible_row: String) {

}
