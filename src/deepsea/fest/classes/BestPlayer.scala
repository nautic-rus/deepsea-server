package deepsea.fest.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

class BestPlayer(val name: String, val disc: String) {

}
object BestPlayer {
  implicit val writes: Writes[BestPlayer] = new Writes[BestPlayer] {
    override def writes(o: BestPlayer): JsValue = o match {
      case x: BestPlayer => Json.obj(
        "name" -> x.name,
        "disc" -> x.disc,
      )
      case _ => JsNull
    }
  }
  implicit val reads: Reads[BestPlayer] = new Reads[BestPlayer] {
    override def reads (json: JsValue): JsResult[BestPlayer] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess
        JsSuccess(new BestPlayer(
          name = (x \ "name").asOpt[String].getOrElse(""),
          disc = (x \ "disc").asOpt[String].getOrElse(""),
        ))
      case _ => JsSuccess (null)
    }
  }
}