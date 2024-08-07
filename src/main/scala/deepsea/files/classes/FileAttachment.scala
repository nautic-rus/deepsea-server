package deepsea.files.classes

import play.api.libs.json._

object FileAttachment{
  implicit val reads: Reads[FileAttachment] = new Reads[FileAttachment] {
    override def reads(json: JsValue): JsResult[FileAttachment] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new FileAttachment(
        name = (x \ "name").asOpt[String].getOrElse(""),
        url = (x \ "url").asOpt[String].getOrElse(""),
        upload_date = (x \ "upload_date").asOpt[Long].getOrElse(0),
        author = (x \ "author").asOpt[String].getOrElse(""),
        revision = (x \ "revision").asOpt[String].getOrElse(""),
        group = (x \ "group").asOpt[String].getOrElse(""),
        cloud = (x \ "cloud").asOpt[Int].getOrElse(0),
      ){
        issue_id = (x \ "issue_id").asOpt[Int].getOrElse(0)
      })
      case _ => JsSuccess(null)
    }
  }
  implicit val writes: Writes[FileAttachment] = new Writes[FileAttachment] {
    override def writes(o: FileAttachment): JsValue = o match {
      case x: FileAttachment => Json.obj(
        "name" -> x.name,
        "url" -> x.url,
        "upload_date" -> x.upload_date,
        "author" -> x.author,
        "revision" -> x.revision,
        "group" -> x.group,
        "issue_id" -> x.issue_id,
        "removed_date" -> x.removed_date,
        "removed_by" -> x.removed_by,
        "cloud" -> x.cloud,
      )
      case _ => JsNull
    }
  }
}
class FileAttachment(val name: String, val url: String, val upload_date: Long, val author: String, val revision: String = "", val group: String = "", val cloud: Int = 0) {
  var issue_id: Int = 0
  var removed_date: Long = 0
  var removed_by: String = ""
}
