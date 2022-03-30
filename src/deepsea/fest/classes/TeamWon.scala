package deepsea.fest.classes

import play.api.libs.json.{JsNull, JsObject, JsResult, JsSuccess, JsValue, Json, Reads, Writes}

class TeamWon(val sport: String, val game: String, val team: String) {

}
object TeamWon {
  implicit val writes: Writes[TeamWon] = new Writes[TeamWon] {
    override def writes(o: TeamWon): JsValue = o match {
      case x: TeamWon => Json.obj(
        "sport" -> x.sport,
        "game" -> x.game,
        "team" -> x.team,
      )
      case _ => JsNull
    }
  }
  implicit val reads: Reads[TeamWon] = new Reads[TeamWon] {
    override def reads (json: JsValue): JsResult[TeamWon] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess
        JsSuccess(new TeamWon(
          sport = (x \ "sport").asOpt[String].getOrElse(""),
          game = (x \ "game").asOpt[String].getOrElse(""),
          team = (x \ "team").asOpt[String].getOrElse(""),
        ))
      case _ => JsSuccess (null)
    }
  }
}