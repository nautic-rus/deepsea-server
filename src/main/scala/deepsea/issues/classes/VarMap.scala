package deepsea.issues.classes

import play.api.libs.json._

object VarMap{
  implicit val reads: Reads[VarMap] = new Reads[VarMap] {
    override def reads(json: JsValue): JsResult[VarMap] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new VarMap(
        name = (x \ "name").asOpt[String].getOrElse(""),
        value = (x \ "value").asOpt[String].getOrElse(""),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[VarMap] = new Writes[VarMap] {
    override def writes(o: VarMap): JsValue = o match {
      case x: VarMap => Json.obj(
        "name" -> x.name,
        "value" -> x.value,
      )
      case _ => JsNull
    }
  }
}
class VarMap(val name: String, val value: String) {

}
