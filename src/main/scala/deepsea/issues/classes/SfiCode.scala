package deepsea.issues.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

object SfiCode{
  implicit val writes: Writes[SfiCode] = new Writes[SfiCode] {
    override def writes(o: SfiCode): JsValue = o match {
      case x: SfiCode => Json.obj(
        "code" -> x.code,
        "ru" -> x.ru,
        "en" -> x.en,
      )
      case _ => JsNull
    }
  }
}
class SfiCode(val code: String, val ru: String, val en: String) {

}
