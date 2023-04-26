package deepsea.issues.classes

import play.api.libs.json._

object IssueView{
  implicit val readsIssueMessage: Reads[IssueView] = new Reads[IssueView] {
    override def reads(json: JsValue): JsResult[IssueView] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueView(
        user = (x \ "user").asOpt[String].getOrElse(""),
        issue = (x \ "issue").asOpt[Int].getOrElse(0),
        date = (x \ "date").asOpt[Long].getOrElse(0),
      ))
      case _ => JsSuccess(null)
    }
  }
  implicit val writesIssueView: Writes[IssueView] = new Writes[IssueView] {
    override def writes(o: IssueView): JsValue = o match {
      case x: IssueView => Json.obj(
        "user" -> x.user,
        "issue" -> x.issue,
        "date" -> x.date,
      )
      case _ => JsNull
    }
  }
}
class IssueView(val user: String, val issue: Int, val date: Long) {
}
