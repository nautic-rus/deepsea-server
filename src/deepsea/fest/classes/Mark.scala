package deepsea.fest.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

class Mark(val disc: String, val team: String, var value: Double) {

}
object Mark {
  implicit val writes: Writes[Mark] = new Writes[Mark] {
    override def writes(o: Mark): JsValue = o match {
      case x: Mark => Json.obj(
        "disc" -> x.disc,
        "team" -> x.team,
        "value" -> x.value,
      )
      case _ => JsNull
    }
  }
  implicit val reads: Reads[Mark] = new Reads[Mark] {
    override def reads (json: JsValue): JsResult[Mark] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess
        JsSuccess(new Mark(
          disc = (x \ "disc").asOpt[String].getOrElse(""),
          team = (x \ "team").asOpt[String].getOrElse(""),
          value = (x \ "value").asOpt[Double].getOrElse(0),
        ))
      case _ => JsSuccess (null)
    }
  }
}