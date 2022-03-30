package deepsea.materials.classes

import play.api.libs.json._

object WSMaterial{
  implicit val writesIssue: Writes[WSMaterial] = new Writes[WSMaterial] {
    override def writes(o: WSMaterial): JsValue = o match {
      case x: WSMaterial => Json.obj(
        "category" -> x.category,
        "coefficient" -> x.coefficient,
        "comment" -> x.comment,
        "description" -> x.description,
        "document" -> x.document,
        "id" -> x.id,
        "name" -> x.name,
        "name" -> x.name,
        "note" -> x.note,
        "project" -> x.project,
        "provider" -> x.provider,
        "removed" -> x.removed,
        "singleWeight" -> x.singleWeight,
        "trmCode" -> x.trmCode,
        "units" -> x.units
      )
      case _ => JsNull
    }
  }
  implicit val readsIssue: Reads[WSMaterial] = new Reads[WSMaterial] {
    override def reads (json: JsValue): JsResult[WSMaterial] = json.asOpt[JsObject] match {
      case Some (x) => JsSuccess (new WSMaterial(
        category = (x \ "category").asOpt[String].getOrElse(""),
        coefficient = (x \ "coefficient").asOpt[Int].getOrElse(0),
        comment = (x \ "comment").asOpt[String].getOrElse(""),
        description = (x \ "description").asOpt[String].getOrElse(""),
        document = (x \ "document").asOpt[String].getOrElse(""),
        id = (x \ "id").asOpt[String].getOrElse(""),
        name = (x \ "name").asOpt[String].getOrElse(""),
        note = (x \ "note").asOpt[String].getOrElse(""),
        project = (x \ "project").asOpt[String].getOrElse(""),
        provider = (x \ "provider").asOpt[String].getOrElse(""),
        removed = (x \ "removed").asOpt[Int].getOrElse(0),
        singleWeight = (x \ "singleWeight").asOpt[Double].getOrElse(0),
        trmCode = (x \ "trmCode").asOpt[String].getOrElse(""),
        units = (x \ "units").asOpt[String].getOrElse("")
      ))
      case _ => JsSuccess (null)
    }
  }
}
class WSMaterial(val category: String,
                 val coefficient: Int,
                 val comment: String,
                 val description: String,
                 val document: String,
                 val id: String,
                 val name: String,
                 val note: String,
                 val project: String,
                 val provider: String,
                 val removed: Int,
                 val singleWeight: Double,
                 val trmCode: String,
                 val units: String) {
}
