package deepsea.issues.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

object IssueHistory{
  implicit val writesIssueHistory: Writes[IssueHistory] = new Writes[IssueHistory] {
    override def writes(o: IssueHistory): JsValue = o match {
      case x: IssueHistory => Json.obj(
        "id" -> x.id,
        "author" -> x.author,
        "name_value" -> x.name_value,
        "prev_value" -> x.prev_value.toString,
        "new_value" -> x.new_value.toString,
        "update_date" -> x.update_date,
        "update_message" -> x.update_message,
      )
      case _ => JsNull
    }
  }
  implicit val readsIssue: Reads[IssueHistory] = new Reads[IssueHistory] {
    override def reads (json: JsValue): JsResult[IssueHistory] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess (new IssueHistory(
        id = (x \ "id").asOpt[Int].getOrElse(0),
        author = (x \ "author").asOpt[String].getOrElse(""),
        name_value = (x \ "name_value").asOpt[String].getOrElse(""),
        prev_value = (x \ "prev_value").asOpt[String].getOrElse(""),
        new_value = (x \ "new_value").asOpt[String].getOrElse(""),
        update_date = (x \ "update_date").asOpt[Long].getOrElse(0),
        update_message = (x \ "update_message").asOpt[String].getOrElse(""),
      ))
      case _ => JsSuccess (null)
    }
  }
}
class IssueHistory(val id: Int, val author: String, val name_value: String, val prev_value: String, val new_value: String, val update_date: Long, val update_message: String)