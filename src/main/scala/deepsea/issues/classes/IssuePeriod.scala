package deepsea.issues.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

object IssuePeriod{
  implicit val reads: Reads[IssuePeriod] = new Reads[IssuePeriod] {
    override def reads(json: JsValue): JsResult[IssuePeriod] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssuePeriod(
        name = (x \ "name").asOpt[String].getOrElse(""),
        project = (x \ "project").asOpt[String].getOrElse(""),
        date = (x \ "date").asOpt[Long].getOrElse(0),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[IssuePeriod] = new Writes[IssuePeriod] {
    override def writes(o: IssuePeriod): JsValue = o match {
      case x: IssuePeriod => Json.obj(
        "name" -> x.name,
        "project" -> x.project,
        "date" -> x.date,
      )
      case _ => JsNull
    }
  }
}
class IssuePeriod(val name: String, val project: String, val date: Long) {

}
