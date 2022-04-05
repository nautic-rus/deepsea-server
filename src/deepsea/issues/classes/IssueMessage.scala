package deepsea.issues.classes

import deepsea.files.classes.FileAttachment
import play.api.libs.json._

import scala.collection.mutable.ListBuffer

object IssueMessage{
  implicit val readsIssueMessage: Reads[IssueMessage] = new Reads[IssueMessage] {
    override def reads(json: JsValue): JsResult[IssueMessage] = json.asOpt[JsObject] match {
      case Some(x) => JsSuccess(new IssueMessage(
        author = (x \ "author").asOpt[String].getOrElse(""),
        content = (x \ "content").asOpt[String].getOrElse(""),
        date = (x \ "date").asOpt[Long].getOrElse(0),
        prefix = (x \ "prefix").asOpt[String].getOrElse(""),
        to_be_replied = (x \ "to_be_replied").asOpt[Int].getOrElse(0)
      ){
        file_attachments ++= (x \ "file_attachments").asOpt[ListBuffer[FileAttachment]].getOrElse(ListBuffer.empty[FileAttachment])
      })
      case _ => JsSuccess(null)
    }
  }
  implicit val writesIssueMessage: Writes[IssueMessage] = new Writes[IssueMessage] {
    override def writes(o: IssueMessage): JsValue = o match {
      case x: IssueMessage => Json.obj(
        "author" -> x.author,
        "content" -> x.content,
        "date" -> x.date,
        "prefix" -> x.prefix,
        "to_be_replied" -> x.to_be_replied,
        "file_attachments" -> x.file_attachments,
      )
      case _ => JsNull
    }
  }
}
class IssueMessage(val author: String, val content: String, val date: Long, val prefix: String = "", val to_be_replied: Int = 0) {
  var file_attachments: ListBuffer[FileAttachment] = ListBuffer.empty[FileAttachment]
}
