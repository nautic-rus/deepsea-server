package deepsea.materials

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.model.headers
import com.mongodb.BasicDBObject
import deepsea.dbase.{DBManager, MongoCodecs}
import deepsea.materials.MaterialManager._
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal}
import play.api.libs.json.{Json, __}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.language.postfixOps
import slick.jdbc.PostgresProfile.api._
import slick.lifted.{ProvenShape, TableQuery}

import java.util.concurrent.TimeUnit
import scala.io.Source

object MaterialManager{
  case class GetMaterials(project: String)
  case class GetMaterialsCode(project: String, code: String)
  case class UpdateMaterial(material: String, user: String, remove: String = "0")
  case class GetMaterialNodes(project: String)
  case class UpdateMaterialNode(project: String, data: String, label: String, label_ru: String, user: String, remove: String = "0")
  case class GetWeightControl()
  case class GetWCDrawings()
  case class GetWCZones()
  case class SetWeightControl(controlValue: String)
  case class RemoveWeightControl(controlValue: String, user: String)
  case class MaterialHistory(material: Material, user: String, date: Long = new Date().getTime)
  case class Material(
                       name: String = "",
                       description: String = "",
                       category: String = "",
                       code: String = "",
                       units: String = "",
                       singleWeight: Double = 0,
                       project: String = "",
                       document: String = "",
                       provider: String = "",
                       note: String = "",
                       comment: String = "",
                       manufacturer: String = "",
                       coefficient: Double = 1,
                       id: String = UUID.randomUUID().toString,
                       translations: List[MaterialTranslation] = List.empty[MaterialTranslation],
                       itt: Int = 0,
                       approved: Int = 0,
                       tp620: Option[Int] = Option(0),
                       certRS: Option[Int] = Option(0),
                       density: Option[Double] = Option(0)) {
    def name(lang: String = "en"): String = {
      translations.find(_.lang == lang) match {
        case Some(translation) => translation.name
        case _ => name
      }
    }
    def description(lang: String = "en"): String = {
      translations.find(_.lang == lang) match {
        case Some(translation) => translation.description
        case _ => description
      }
    }
  }
  case class ProjectName(id: String, rkd: String, pdsp: String, foran: String, cloud: String, cloudRkd: String)
  case class MaterialTranslation(lang: String, name: String, description: String)
  case class MaterialNode(project: String, label: String, label_ru: String, data: String, user: String, date: Long)
  case class MaterialNodeHistory(node: MaterialNode, user: String, date: Long = new Date().getTime)

  case class WeightControl(docNumber: String, docName: String, zoneNumber: String, moveElement: String, zoneName: String, mount: Int, side: Int, weight: Double, x: Double, y: Double, z: Double, user: String, date: Long, project: String, var removedDate: Long, var removedUser: String)
  case class WCNumberName(number: String, name: String, project: String)


  case class MaterialComplect(id: String, project: Int, name: String, kind: String, user_id: Int, date: Long, materials: List[CMaterial])
  case class CMaterial(material: String, count: Double)
  case class GetMaterialComplects(project: String)
  case class AddMaterialComplect(project: String, name: String, kind: String, user_id: String)
  case class RemoveMaterialComplect(id: String)
  case class UpdateMaterialComplect(complectValue: String)




  case class GetEquipments()
  case class GetEquipment(id: String)
  case class GetSFIs()
  case class InsertEquipment(jsonValue: String)
  case class DeleteEquipment(id: Int)
  case class InsertSupplier(jsonValue: String)
  case class DeleteSupplier(id: Int)
  case class GetEquipFiles(id: Int)
  case class AddEquipFile(jsonValue: String)
  case class DelEquipFile(id: Int)

  case class GetSupFiles(id: Int)
  case class AddSupFile(jsonValue: String)
  case class DelSupFile(id: Int)

  case class GetRelatedTasks(id: Int)
  case class DelRelatedTask(id: Int)

  case class GetSupplierHistory(id: Int)
  case class AddSupplierHistory(jsonValue: String)

  case class Equipment(id: Int, sfi: String, name: String, description: String, department: String, comment: String, responsible_id: Int, respons_name: String, respons_surname: String, itt: Int, project_name: String, sfi_unit: String, parent_id: Int, create_date: Long, suppliers: List[Supplier])
  implicit val EquipmentDecoder: Decoder[Equipment] = deriveDecoder[Equipment]
  implicit val EquipmentEncoder: Encoder[Equipment] = deriveEncoder[Equipment]

  case class EquipmentAdd(id: Int, name: String, description: String, sfi: String, project_id: Int, responsible_id: Int, department_id: Int, comment: String, status_id: Int, sfi_unit: String, parent_id: Int)
  implicit val EquipmentAddDecoder: Decoder[EquipmentAdd] = deriveDecoder[EquipmentAdd]
  implicit val EquipmentAddEncoder: Encoder[EquipmentAdd] = deriveEncoder[EquipmentAdd]

  case class Supplier(id: Int, user_id: Int, equip_id: Int, name: String, sup_id: Int, description: String, comment: String, manufacturer: String, status: String, approvement: Long, last_update: Long, model: String, ele_param: String, mech_param: String, weight: Double, status_id: Int)
  implicit val SupplierDecoder: Decoder[Supplier] = deriveDecoder[Supplier]
  implicit val SupplierEncoder: Encoder[Supplier] = deriveEncoder[Supplier]

  case class SupplierAdd(id: Int, approvement: Long, comment: String, status_id: Int, equip_id: Int, user_id: Int, sup_id: Int, manufacturer: String, description: String, model: String, ele_param: String, mech_param: String, weight: Double)
  implicit val SupplierAddDecoder: Decoder[SupplierAdd] = deriveDecoder[SupplierAdd]
  implicit val SupplierAddEncoder: Encoder[SupplierAdd] = deriveEncoder[SupplierAdd]

  case class SFI(code: String, ru: String, eng: String)
  implicit val SFIDecoder: Decoder[SFI] = deriveDecoder[SFI]
  implicit val SFIEncoder: Encoder[SFI] = deriveEncoder[SFI]

  case class EquipFile(equ_id: Int, user_id: Int, url: String, rev: String, archived: Int, create_date: Long, type_name: String, id: Int, archived_date: Long)
  implicit val EquipFileDecoder: Decoder[EquipFile] = deriveDecoder[EquipFile]
  implicit val EquipFileEncoder: Encoder[EquipFile] = deriveEncoder[EquipFile]

  case class EquipFileAdd(equ_id: Int, url: String, rev: String, type_name: String, user_id: Int)
  implicit val EquipFileAddDecoder: Decoder[EquipFileAdd] = deriveDecoder[EquipFileAdd]
  implicit val EquipFileAddEncoder: Encoder[EquipFileAdd] = deriveEncoder[EquipFileAdd]

  case class SuppFile(supplier_id: Int, user_id: Int, url: String, rev: String, archived: Int, create_date: Long, type_name: String, id: Int, archived_date: Long)
  implicit val SuppFileDecoder: Decoder[SuppFile] = deriveDecoder[SuppFile]
  implicit val SuppFileEncoder: Encoder[SuppFile] = deriveEncoder[SuppFile]

  case class SuppFileAdd(supplier_id: Int, url: String, rev: String, type_name: String, user_id: Int)
  implicit val SuppFileAddDecoder: Decoder[SuppFileAdd] = deriveDecoder[SuppFileAdd]
  implicit val SuppFileAddEncoder: Encoder[SuppFileAdd] = deriveEncoder[SuppFileAdd]

  case class SupplierHistory(name_value: String, prev_value: String, new_value: String, author: Int, supplier_id: Int, update_date: Long)
  implicit val SupplierHistoryDecoder: Decoder[SupplierHistory] = deriveDecoder[SupplierHistory]
  implicit val SupplierHistoryEncoder: Encoder[SupplierHistory] = deriveEncoder[SupplierHistory]

  case class SupplierHistoryAdd(value: String, old_value: String, new_value: String, user_id: Int, supplier_id: Int)
  implicit val SupplierHistoryAddDecoder: Decoder[SupplierHistoryAdd] = deriveDecoder[SupplierHistoryAdd]
  implicit val SupplierHistoryAddEncoder: Encoder[SupplierHistoryAdd] = deriveEncoder[SupplierHistoryAdd]

  case class RelatedTask(id: Int, issue_id: Int, issue_typ: String, issue_name: String, started_by: String, responsible: String, assigned_to: String, status: String)
  implicit val RelatedTaskDecoder: Decoder[RelatedTask] = deriveDecoder[RelatedTask]
  implicit val RelatedTaskEncoder: Encoder[RelatedTask] = deriveEncoder[RelatedTask]


  case class SpecMaterial(code: String, name: String, descr: String, units: String, weight: Double, supplier: String, statem_id: Int, dir_id: Int, user_id: Int, label: String, last_upd: Long, note: String, manufacturer: String, coef: Double, id: Int, removed: Int)
  class SpecMaterialTable(tag: Tag) extends Table[SpecMaterial](tag, "materials") {
    val code = column[String]("stock_code")
    val name = column[String]("name")
    val descr = column[String]("description")
    val units = column[String]("unit")
    val weight = column[Double]("weight")
    val supplier = column[String]("supplier")
    val statem_id = column[Int]("statement_id")
    val dir_id = column[Int]("directory_id")
    val user_id = column[Int]("user_id")
    val label = column[String]("default_label")
    val last_upd = column[Long]("last_update")
    val note = column[String]("note")
    val manufacturer = column[String]("manufacturer")
    val coef = column[Double]("coef")
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val removed = column[Int]("removed")
    override def * = (code, name, descr, units, weight, supplier, statem_id, dir_id, user_id, label, last_upd, note, manufacturer, coef, id, removed) <> ((SpecMaterial.apply _).tupled, SpecMaterial.unapply)
  }
  implicit val SpecMaterialDecoder: Decoder[SpecMaterial] = deriveDecoder[SpecMaterial]
  implicit val SpecMaterialEncoder: Encoder[SpecMaterial] = deriveEncoder[SpecMaterial]


  case class MaterialDirectory(id: Int, name: String, parent_id: Int, user_id: Int, date: Long, old_code: String, project_id: Int, removed: Int)
  class MaterialDirectoryTable(tag: Tag) extends Table[MaterialDirectory](tag, "materials_directory") {
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val name = column[String]("name")
    val parent_id = column[Int]("parent_id")
    val user_id = column[Int]("user_id")
    val date = column[Long]("date")
    val old_code = column[String]("old_code")
    val project_id = column[Int]("project_id")
    val removed = column[Int]("removed")
    override def * = (id, name, parent_id, user_id, date, old_code, project_id, removed) <> ((MaterialDirectory.apply _).tupled, MaterialDirectory.unapply)
  }
  implicit val MaterialDirectoryDecoder: Decoder[MaterialDirectory] = deriveDecoder[MaterialDirectory]
  implicit val MaterialDirectoryEncoder: Encoder[MaterialDirectory] = deriveEncoder[MaterialDirectory]

  case class MaterialStatement(id: Int, name: String, project_id: Int, code: String, parent_id: Int, doc_number: String)
  class MaterialStatementTable(tag: Tag) extends Table[MaterialStatement](tag, "materials_statements") {
    val id = column[Int]("id", O.AutoInc)
    val name = column[String]("name")
    val project_id = column[Int]("project_id")
    val code = column[String]("code")
    val parent_id = column[Int]("parent_id")
    val doc_number = column[String]("doc_number")
    override def * = (id, name, project_id, code, parent_id, doc_number) <> ((MaterialStatement.apply _).tupled, MaterialStatement.unapply)
  }
  implicit val MaterialStatementDecoder: Decoder[MaterialStatement] = deriveDecoder[MaterialStatement]
  implicit val MaterialStatementEncoder: Encoder[MaterialStatement] = deriveEncoder[MaterialStatement]

  case class SupMatRelations(id: Int, supplier_id: Int, materials_id: Int)
  class SupMatRelationsTable(tag: Tag) extends Table[SupMatRelations](tag, "sup_mat_relations") {
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val supplier_id = column[Int]("supplier_id")
    val materials_id = column[Int]("materials_id")
    override def * = (id, supplier_id, materials_id) <> ((SupMatRelations.apply _).tupled, SupMatRelations.unapply)
  }
  implicit val SupMatRelationsDecoder: Decoder[SupMatRelations] = deriveDecoder[SupMatRelations]
  implicit val SupMatRelationsEncoder: Encoder[SupMatRelations] = deriveEncoder[SupMatRelations]

  case class EqSupMatRelations(materials_id: Int, name: String, stock_code: String, doc_number: String, issue_id: Int, equ_id: Int, dep_name: String, foran: String)
  implicit val EqSupMatRelationsDecoder: Decoder[EqSupMatRelations] = deriveDecoder[EqSupMatRelations]
  implicit val EqSupMatRelationsEncoder: Encoder[EqSupMatRelations] = deriveEncoder[EqSupMatRelations]

  case class GetSpecMaterials()
  case class GetSpecDirectories()
  case class GetSpecStatements()
  case class GetSupStatuses()
  case class UpdateMaterials()
  case class UpdateSpecMaterial(json: String)
  case class UpdateMaterialDirectory(json: String)


  case class SupTaskRelations(id: Int, suppliers_id: Int, task_id: Int)
  class SupTaskRelationsTable(tag: Tag) extends Table[SupTaskRelations](tag, "sup_task_relations") {
    val id = column[Int]("id", O.AutoInc)
    val suppliers_id = column[Int]("suppliers_id")
    val task_id = column[Int]("task_id")
    override def * = (id, suppliers_id, task_id) <> ((SupTaskRelations.apply _).tupled, SupTaskRelations.unapply)
  }
  implicit val SupTaskRelationsDecoder: Decoder[SupTaskRelations] = deriveDecoder[SupTaskRelations]
  implicit val SupTaskRelationsEncoder: Encoder[SupTaskRelations] = deriveEncoder[SupTaskRelations]


  case class SupName(id: Int, name: String)
  class SupNameTable(tag: Tag) extends Table[SupName](tag, "suppliers_name") {
    val id = column[Int]("id", O.AutoInc)
    val name = column[String]("name")
    override def * = (id, name) <> ((SupName.apply _).tupled, SupName.unapply)
  }
  implicit val SupNameDecoder: Decoder[SupName] = deriveDecoder[SupName]
  implicit val SupNameEncoder: Encoder[SupName] = deriveEncoder[SupName]


  case class SupStatus(id: Int, name: String)
  class SupStatusTable(tag: Tag) extends Table[SupStatus](tag, "suppliers_status") {
    val name = column[String]("name")
    val id = column[Int]("id", O.AutoInc)
    override def * = (id, name) <> ((SupStatus.apply _).tupled, SupStatus.unapply)
  }
  implicit val SupStatusDecoder: Decoder[SupStatus] = deriveDecoder[SupStatus]
  implicit val SupStatusEncoder: Encoder[SupStatus] = deriveEncoder[SupStatus]


  case class SupTaskAdd(jsonValue: String)
  case class GetSupNames()
  case class AddSupName(jsonValue: String)
  case class GetSupMatRelations()
  case class AddSupMatRelation(jsonValue: String)
  case class GetEqSupMatRelations(supId: String)

}
class MaterialManager extends Actor with MongoCodecs with MaterialManagerHelper {

  val collection = "materials-n"
  val collectionNodes = "materials-n-nodes"
  val collectionNodesHistory = "materials-n-nodes-h"
  val collectionHistory = "materials-n-h"


  override def preStart(): Unit = {

    //self ! GetEquipFiles(0)
    //self ! GetEquipments()
    //self ! GetSpecMaterials()
    //self ! UpdateMaterials()

  }
  override def receive: Receive = {
    case GetMaterialNodes(project) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection(collectionNodes).find[MaterialNode](equal("project", project)).toFuture(), Duration(30, SECONDS)) match {
            case nodes => sender() ! nodes.toList.asJson.noSpaces
            case _ => List.empty[MaterialNode]
          }
        case _ => List.empty[MaterialNode]
      }
    case UpdateMaterialNode(project, data, label, label_ru, user, remove) =>
      val node = MaterialNode(project, label, label_ru, data, user, new Date().getTime)
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val nodes: MongoCollection[MaterialNode] = mongo.getCollection(collectionNodes)
          val nodesHistory: MongoCollection[MaterialNodeHistory] = mongo.getCollection(collectionNodesHistory)
          Await.result(nodes.find(and(equal("data", node.data), equal("project", project))).first().toFuture(), Duration(30, SECONDS)) match {
            case oldValue: MaterialNode =>
              Await.result(nodesHistory.insertOne(MaterialNodeHistory(oldValue, user)).toFuture(), Duration(30, SECONDS))
              Await.result(nodes.deleteOne(and(equal("data", node.data), equal("project", project))).toFuture(), Duration(30, SECONDS))
            case _ =>
          }
          if (remove == "0"){
            Await.result(nodes.insertOne(node).toFuture(), Duration(30, SECONDS))
          }
        case _ =>
      }
      sender() ! Json.toJson("success")
    case GetMaterials(project) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection(collection).find[Material](new BasicDBObject("project", project)).toFuture(), Duration(30, SECONDS)) match {
            case dbMaterials =>
              val errors = ListBuffer.empty[String]
              dbMaterials.toList.groupBy(_.code).toList.foreach(g => {
                if (g._2.length > 1){
                  errors += g._1
                }
              })
              sender() ! dbMaterials.toList.asJson.noSpaces
            case _ => List.empty[Material]
          }
        case _ => List.empty[Material]
      }
    case GetMaterialsCode(project, code) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection(collection).find[Material](new BasicDBObject("project", project)).toFuture(), Duration(30, SECONDS)) match {
            case dbMaterials =>
              val errors = ListBuffer.empty[String]
              sender() ! dbMaterials.toList.filter(_.code == code).asJson.noSpaces
            case _ => List.empty[Material]
          }
        case _ => List.empty[Material]
      }
    case UpdateMaterial(materialValue, user, remove) =>
      decode[Material](materialValue) match {
        case Right(material) =>
          DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              val materials: MongoCollection[Material] = mongo.getCollection(collection)
              val materialsHistory: MongoCollection[MaterialHistory] = mongo.getCollection(collectionHistory)
              Await.result(mongo.getCollection(collection).find[Material](new BasicDBObject("id", material.id)).first().toFuture(), Duration(30, SECONDS)) match {
                case oldMaterial: Material =>
                  Await.result(materialsHistory.insertOne(MaterialHistory(oldMaterial, user)).toFuture(), Duration(30, SECONDS))
                  Await.result(materials.deleteOne(new BasicDBObject("id", material.id)).toFuture(), Duration(30, SECONDS))
                case _ =>
              }
              if (remove == "0"){
                Await.result(materials.insertOne(material).toFuture(), Duration(30, SECONDS))
              }
            case _ =>
          }
        case Left(value) =>
      }
      sender() ! Json.toJson("success")
    case GetWeightControl() =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("weightControl").find[WeightControl]().toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.filter(_.removedDate == 0).asJson.noSpaces
            case _ => List.empty[WeightControl]
          }
        case _ => List.empty[WeightControl]
      }
    case SetWeightControl(controlValue) =>
      decode[WeightControl](controlValue) match {
        case Right(weightControl) =>
          DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              val controls: MongoCollection[WeightControl] = mongo.getCollection("weightControl")
              Await.result(controls.insertOne(weightControl).toFuture(), Duration(30, SECONDS))
            case _ =>
          }
        case Left(value) =>
      }
      sender() ! Json.toJson("success")
    case RemoveWeightControl(controlValue, user) =>
      decode[WeightControl](controlValue) match {
        case Right(weightControl) =>
          DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              val controls: MongoCollection[WeightControl] = mongo.getCollection("weightControl")
              weightControl.removedUser = user
              weightControl.removedDate = new Date().getTime
              Await.result(controls.replaceOne(and(equal("date", weightControl.date), equal("user", weightControl.user)), weightControl).toFuture(), Duration(30, SECONDS))
            case _ =>
          }
        case Left(value) =>
      }
      sender() ! Json.toJson("success")
    case GetWCDrawings() =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("wcDrawings").find[WCNumberName]().toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[WCNumberName]
          }
        case _ => List.empty[WCNumberName]
      }
    case GetWCZones() =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("wcZones").find[WCNumberName]().toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[WCNumberName]
          }
        case _ => List.empty[WCNumberName]
      }

    case GetMaterialComplects(project) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("material-complects").find[MaterialComplect](equal("project", project.toIntOption.getOrElse(0))).toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[MaterialComplect]
          }
        case _ => List.empty[WCNumberName]
      }
    case AddMaterialComplect(project, name, kind, user_id) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val complects: MongoCollection[MaterialComplect] = mongo.getCollection("material-complects")
          Await.result(complects.insertOne(MaterialComplect(UUID.randomUUID().toString, project.toIntOption.getOrElse(0), name, kind, user_id.toIntOption.getOrElse(0), new Date().getTime, List.empty[CMaterial])).toFuture(), Duration(30, SECONDS))
        case _ =>
      }
      sender() ! "success".asJson.noSpaces
    case RemoveMaterialComplect(id) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val complects: MongoCollection[MaterialComplect] = mongo.getCollection("material-complects")
          Await.result(complects.deleteOne(equal("id", id)).toFuture(), Duration(30, SECONDS))
        case _ =>
      }
      sender() ! "success".asJson.noSpaces
    case UpdateMaterialComplect(complectValue) =>
      decode[MaterialComplect](complectValue) match {
        case Right(complect) =>
          DBManager.GetMongoConnection() match {
            case Some(mongo) =>
              val complects: MongoCollection[MaterialComplect] = mongo.getCollection("material-complects")
              Await.result(complects.replaceOne(equal("id", complect.id), complect).toFuture(), Duration(30, SECONDS))
            case _ =>
          }
        case Left(value) =>
      }
      sender() ! Json.toJson("success")

    case GetEquipments() =>
      sender() ! getEquipments(getSuppliers).asJson.noSpaces
    case GetEquipment(id) =>
      sender() ! getEquipment(id.toIntOption.getOrElse(0), getSuppliers).asJson.noSpaces
    case GetSFIs() =>
      sender() ! getSFIs.asJson.noSpaces
    case InsertEquipment(jsonValue) =>
      val res: String = decode[EquipmentAdd](jsonValue) match {
        case Right(eq) =>
          DBManager.GetPGConnection() match {
            case Some(pg) =>
              val stmt = pg.createStatement()
              try{
                val date = new Date().getTime
                var eqId = eq.id
                if (eq.id == 0) {
                  val query = s"insert into equipments values (default, '${eq.name}', '${eq.description}', '-', ${eq.project_id}, ${eq.responsible_id}, ${eq.department_id}, $date, '${eq.comment}', ${eq.status_id}, 0, '${eq.sfi_unit}', ${eq.parent_id}) returning id"
                  val rs = stmt.executeQuery(query)
                  while (rs.next()) {
                    eqId = rs.getInt("id")
                  }
                  rs.close()
                }
                else{
                  val query = s"update equipments set name = '${eq.name}', descriptions = '${eq.description}', sfi = '-', project_id = ${eq.project_id}, responsible_id = ${eq.responsible_id}, department_id = ${eq.department_id}, comment = '${eq.comment}', status_id = ${eq.status_id}, sfi_unit = '${eq.sfi_unit}', parent_id = ${eq.parent_id} where id = ${eq.id}"
                  stmt.execute(query)
                }
                pg.close()
                eqId.toString
              }
              catch {
                case e: Exception =>
                  stmt.close()
                  pg.close()
                  "error: " + e.toString
              }
            case _ => "error: no database connection"
          }
        case Left(value) => "error: wrong post json value"
      }
      sender() ! res.asJson.noSpaces
    case DeleteEquipment(id) =>
      val res = DBManager.GetPGConnection() match {
        case Some(pg) =>
          val stmt = pg.createStatement()
          try {
            val query = s"update equipments set removed = 1 where id = $id"
            stmt.execute(query)
            stmt.close()
            pg.close()
            "success"
          }
          catch {
            case e: Exception =>
              stmt.close()
              pg.close()
              "error: " + e.toString
          }
        case _ => "error: no database connection"
      }
      sender() ! res.asJson.noSpaces

    case InsertSupplier(jsonValue) =>
      val d = new Date().getTime
      val res: String = decode[Supplier](jsonValue) match {
        case Right(sup) =>
          DBManager.GetPGConnection() match {
            case Some(pg) =>
              val stmt = pg.createStatement()
              try {
                var supId = sup.id
                if (sup.id == 0){
                  val query = s"insert into suppliers values (default, ${sup.user_id}, ${sup.equip_id}, '${sup.description}', '${sup.comment}', ${sup.status_id}, '${sup.manufacturer}', ${sup.approvement}, 0, $d, ${sup.sup_id}, '${sup.model}', '${sup.ele_param}', '${sup.mech_param}', ${sup.weight}) returning id"
                  val rs = stmt.executeQuery(query)
                  while (rs.next()) {
                    supId = rs.getInt("id")
                  }
                  rs.close()
                }
                else {
                  val query = s"update suppliers set user_id = ${sup.user_id}, equ_id = ${sup.equip_id}, description = '${sup.description}', comment = '${sup.comment}', status_id = ${sup.status_id}, manufacturer = '${sup.manufacturer}', approvement = ${sup.approvement}, last_update = $d, suppliers_name_id = ${sup.sup_id}, model = '${sup.model}', ele_param = '${sup.ele_param}', mech_param = '${sup.mech_param}', weight = '${sup.weight}' where id = ${sup.id}"
                  stmt.execute(query)
                }
                stmt.close()
                pg.close()
                supId.toString
              }
              catch {
                case e: Exception =>
                  stmt.close()
                  pg.close()
                  "error: " + e.toString
              }
            case _ => "error: no database connection"
          }
        case Left(value) => "error: wrong post json value"
      }
      sender() ! res.asJson.noSpaces
    case DeleteSupplier(id) =>
      val res = DBManager.GetPGConnection() match {
        case Some(pg) =>
          val stmt = pg.createStatement()
          try {
            val query = s"update suppliers set removed = 1 where id = $id"
            stmt.execute(query)
            stmt.close()
            pg.close()
            "success"
          }
          catch {
            case e: Exception =>
              stmt.close()
              pg.close()
              "error: " + e.toString
          }
        case _ => "error: no database connection"
      }
      sender() ! res.asJson.noSpaces
    case GetEquipFiles(id) =>
      sender() ! getEquipFiles(id).asJson.noSpaces
    case AddEquipFile(jsonValue) =>
      decode[List[EquipFileAdd]](jsonValue) match {
        case Right(eq) => sender() ! addEquipFile(eq).asJson.noSpaces
        case Left(value) => decode[EquipFileAdd](jsonValue) match {
          case Right(eq) => sender() ! addEquipFile(List(eq)).asJson.noSpaces
          case Left(value) => sender() ! "error: wrong post json value"
        }
      }
    case DelEquipFile(id) =>
      sender() ! delEquipFile(id).asJson.noSpaces
    case GetSupFiles(id) =>
      sender() ! getSupFiles(id).asJson.noSpaces
    case AddSupFile(jsonValue) =>
      decode[List[SuppFileAdd]](jsonValue) match {
        case Right(sup) => sender() ! addSupFile(sup).asJson.noSpaces
        case Left(value) => decode[SuppFileAdd](jsonValue) match {
          case Right(sup) => sender() ! addSupFile(List(sup)).asJson.noSpaces
          case Left(value) => sender() ! "error: wrong post json value"
        }
      }
    case DelSupFile(id) =>
      sender() ! delSupFile(id).asJson.noSpaces
    case GetRelatedTasks(id) =>
      sender() ! getRelatedTasks(id).asJson.noSpaces
    case DelRelatedTask(id) =>
      sender() ! delRelatedTask(id).asJson.noSpaces
    case AddSupplierHistory(jsonValue) =>
      sender() ! (decode[SupplierHistoryAdd](jsonValue) match {
        case Right(sup) => addSupHistory(sup).asJson.noSpaces
        case Left(value) => "error: wrong post json value"
      })
    case GetSupplierHistory(id) =>
      sender() ! getSupplierHistory(id).asJson.noSpaces
    case GetSpecMaterials() =>
      sender() ! (Await.result(getSpecMaterials, Duration(5, SECONDS)) match {
        case response: List[SpecMaterial] => response.asJson.noSpaces
        case _ => "error: wrong sql query".asJson.noSpaces
      })
    case GetSpecDirectories() =>
      sender() ! (Await.result(getMaterialDirectories, Duration(5, SECONDS)) match {
        case response: List[MaterialDirectory] => response.asJson.noSpaces
        case _ => "error: wrong sql query".asJson.noSpaces
      })
    case GetSpecStatements() =>
      sender() ! (Await.result(getMaterialStatements, Duration(5, SECONDS)) match {
        case response: List[MaterialStatement] => response.asJson.noSpaces
        case _ => "error: wrong sql query".asJson.noSpaces
      })
    case GetSupStatuses() =>
      sender() ! (Await.result(getSupStatuses, Duration(5, SECONDS)) match {
        case response: List[SupStatus] => response.asJson.noSpaces
        case _ => "error: wrong sql query".asJson.noSpaces
      })
    case SupTaskAdd(jsonValue) =>
      sender() ! (decode[SupTaskRelations](jsonValue) match {
        case Right(supTask) =>
          Await.result(supTaskAdd(supTask), Duration(5, SECONDS)) match {
            case response: Int => response.asJson.noSpaces
            case _ => "error: wrong sql query".asJson.noSpaces
          }
        case Left(error) => "error: wrong post data".asJson.noSpaces
      })
    case UpdateSpecMaterial(jsonValue) =>
      sender() ! (decode[SpecMaterial](jsonValue) match {
        case Right(value) =>
          updateSpecMaterial(value.copy(last_upd = new Date().getTime))
          "success".asJson.noSpaces
        case Left(error) => "error: wrong post data".asJson.noSpaces
      })
    case UpdateMaterialDirectory(jsonValue) =>
      sender() ! (decode[MaterialDirectory](jsonValue) match {
        case Right(value) =>
          updateMaterialDirectory(value.copy(date = new Date().getTime))
          "success".asJson.noSpaces
        case Left(error) => "error: wrong post data".asJson.noSpaces
      })
    case GetSupNames() =>
      sender() ! (Await.result(getSupNames, Duration(5, SECONDS)) match {
        case response: List[SupName] => response.asJson.noSpaces
        case _ => "error: wrong sql query".asJson.noSpaces
      })
    case AddSupName(jsonValue) =>
      sender() ! (decode[SupName](jsonValue) match {
        case Right(value) =>
          Await.result(addSupName(value), Duration(5, SECONDS)) match {
            case response: Int => response.asJson.noSpaces
            case _ => "error: wrong sql query".asJson.noSpaces
          }
        case Left(error) => "error: wrong post data".asJson.noSpaces
      })
    case GetSupMatRelations() =>
      sender() ! (Await.result(getSupMatRelations, Duration(5, SECONDS)) match {
        case response: List[SupMatRelations] => response.asJson.noSpaces
        case _ => "error: wrong sql query".asJson.noSpaces
      })
    case AddSupMatRelation(jsonValue) =>
      sender() ! (decode[SupMatRelations](jsonValue) match {
        case Right(value) =>
          addSupMatRelation(value)
          "success".asJson.noSpaces
        case Left(error) => "error: wrong post data".asJson.noSpaces
      })
    case GetEqSupMatRelations(supId) =>
      sender() ! getEqSupMatRelations(supId).asJson.noSpaces
    case UpdateMaterials() =>
      //updateMaterials()
    case _ => None
  }
  def getSupNames: Future[List[SupName]] = {
    DBManager.PostgresSQL.run(TableQuery[SupNameTable].result).map(_.toList)
  }
  def getSpecMaterials: Future[List[SpecMaterial]] = {
    DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].filter(_.removed === 0).result).map(_.toList)
  }
  def updateSpecMaterial(specMaterial: SpecMaterial) = {
    val material = if (specMaterial.id == 0){
      val id = Await.result(DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].map(_.id).max.result), Duration(5, SECONDS)) match {
        case response: Option[Int] =>
          val ind = response match {
            case Some(value) => value
            case _ => 0
          }
          alz((ind + 1).toString)
        case _ => alz(0.toString)
      }
      specMaterial.copy(code = "NR" + id)
    }
    else{
      specMaterial
    }
    DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].insertOrUpdate(material))
  }
  def alz(input: String, length: Int = 14) = {
    ("0" * length + input).takeRight(length)
  }
  def getMaterialDirectories: Future[List[MaterialDirectory]] = {
    DBManager.PostgresSQL.run(TableQuery[MaterialDirectoryTable].filter(_.removed === 0).result).map(_.toList)
  }
  def updateMaterialDirectory(materialDirectory: MaterialDirectory) = {
    DBManager.PostgresSQL.run(TableQuery[MaterialDirectoryTable].insertOrUpdate(materialDirectory))
  }
  def getMaterialStatements: Future[List[MaterialStatement]] = {
    DBManager.PostgresSQL.run(TableQuery[MaterialStatementTable].result).map(_.toList)
  }
  def getSupStatuses: Future[List[SupStatus]] = {
    DBManager.PostgresSQL.run(TableQuery[SupStatusTable].result).map(_.toList)
  }
  def getSupMatRelations: Future[List[SupMatRelations]] = {
    DBManager.PostgresSQL.run(TableQuery[SupMatRelationsTable].result).map(_.toList)
  }
  def addSupMatRelation(supMatRelation: SupMatRelations) = {
    DBManager.PostgresSQL.run(TableQuery[SupMatRelationsTable].insertOrUpdate(supMatRelation))
  }
  def updateDirectories(): Unit = {
    getSpecMaterials.onComplete {
      case Success(specMaterials) =>
        val withLabels = specMaterials
        val origMaterials = getMaterials
        val origDirectories = getNodes.filter(_.project == "200101")

        val root = origDirectories.filter(_.data.length == 3)
        val s2 = origDirectories.filter(_.data.length == 6)
        val s3 = origDirectories.filter(_.data.length == 9)
        val s4 = origDirectories.filter(_.data.length == 12)


        val table = TableQuery[MaterialDirectoryTable]

        //root.map(r => DBManager.PostgresSQL.run((table returning table.map(_.id)) += MaterialDirectory(0, r.label, 0, 0, 0, r.data)))

        getMaterialDirectories.onComplete{
          case Success(directories) =>
            s4.map(r => {
              directories.filter(x => x.old_code.length == r.data.length - 3).find(d => r.data.contains(d.old_code)) match {
                case Some(parent) =>
                  DBManager.PostgresSQL.run(table += MaterialDirectory(0, r.label, parent.id, 0, 0, r.data, 0, 0))
                case _ => None

              }
            })
            val qw = 0
          case Failure(exception) => println(exception)
        }
      case Failure(exception) => exception.toString
    }
  }
  def updateMaterialsLang(): Unit = {
    val origMaterials = getMaterials.filter(_.project == "210101")
    val materials = Await.result(getSpecMaterials, Duration(15, SECONDS)) match {
      case response: List[SpecMaterial] => response
      case _ => List.empty[SpecMaterial]
    }
    val statems = Await.result(getMaterialStatements, Duration(15, SECONDS)) match {
      case response: List[MaterialStatement] => response
      case _ => List.empty[MaterialStatement]
    }
    val projectStatems = statems.filter(_.project_id == 2)
    materials.foreach(m => {
      if (projectStatems.contains(m.statem_id)){
        origMaterials.find(_.code == m.code) match {
          case Some(value) =>
            val named = m.copy(name = value.name(), descr = value.description())
            DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].update(named))
          case _ => None
        }
      }
    })
  }

  def updateMaterials(): Unit = {
    getMaterialDirectories.onComplete{
      case Success(directories) =>
        val origMaterials = getMaterials
        val origNodes = getNodes
        val labels = decode[List[SpecMaterial]](Source.fromResource("materials_label.json").mkString) match {
          case Right(value) => value
          case Left(value) => List.empty[SpecMaterial]
        }
        val issueProjects = getIssueProjects
        val upd = new Date().getTime
        val updDirectories: List[MaterialDirectory] = Await.result(getMaterialDirectories, Duration(15, SECONDS)) match {
          case response: List[MaterialDirectory] => response
          case _ => List.empty[MaterialDirectory]
        }
        getMaterialStatements.onComplete {
          case Success(statements) =>
            origMaterials.map(m => {
              val rootNames = origNodes.filter(x =>  x.project == m.project && m.code.contains(x.data)).sortBy(_.data.length)
              rootNames.foreach(rn => {
                updDirectories.find(_.old_code == rn.data) match {
                  case Some(dir) => None
                  case _ =>
                    val ind = rootNames.indexOf(rn)
                    val parentId = if (ind == 0) 0 else{
                      val prev = rootNames(ind - 1)
                      updDirectories.find(_.old_code == prev.data) match {
                        case Some(value) => value.id
                        case _ => -1
                      }
                    }
                    if (parentId == -1){
                      val error = 0
                    }
                    val projectId = issueProjects.find(_.rkd == m.project) match {
                      case Some(value) => value.id
                      case _ => 0
                    }
                    Await.result(DBManager.PostgresSQL.run(TableQuery[MaterialDirectoryTable] += MaterialDirectory(0, rn.label, parentId, 0, 0, rn.data, projectId, 0)), Duration(5, SECONDS))
                }
              })


              val stId = updDirectories.filter(_.old_code != "").sortBy(_.old_code.length).filter(x => x.project_id == (if (m.project == "210101") 2 else 1)).find(x => m.code.contains(x.old_code)) match {
                case Some(dir) =>
                  var dirTop = dir
                  while (dirTop.parent_id != 0){
                    dirTop = updDirectories.find(_.id == dirTop.parent_id).get
                  }
                  statements.find(x => x.name.contains(dirTop.name) || dirTop.name.contains(x.name)) match {
                    case Some(stmt) => stmt.id
                    case _ =>
                      if (m.project == "200101"){
                        100000
                      }
                      else if (m.project == "210101"){
                        100001
                      }
                      else{
                        0
                      }
                  }
                case _ => updDirectories.filter(_.old_code != "").sortBy(_.old_code.length).find(x => m.code.contains(x.old_code)) match {
                  case Some(dir) =>
                    var dirTop = dir
                    while (dirTop.parent_id != 0){
                      dirTop = updDirectories.find(_.id == dirTop.parent_id).get
                    }
                    statements.find(x => x.name.contains(dirTop.name) || dirTop.name.contains(x.name)) match {
                      case Some(stmt) => stmt.id
                      case _ =>
                        if (m.project == "200101"){
                          100000
                        }
                        else if (m.project == "210101"){
                          100001
                        }
                        else{
                          0
                        }
                    }
                  case _ => 0
                }
              }
              if (stId == 0){
                val error = 1
              }
              val dirId = updDirectories.sortBy(_.old_code.length).findLast(x => m.code.contains(x.old_code)) match {
                case Some(value) => value.id
                case _ => 0
              }
              val userId = labels.find(_.code == m.code) match {
                case Some(value) => value.user_id
                case _ => 0
              }
              val label = labels.find(_.code == m.code) match {
                case Some(value) => value.label
                case _ => ""
              }
              SpecMaterial(
                m.code,
                m.name,
                m.description,
                m.units,
                m.singleWeight,
                m.provider,
                stId,
                dirId,
                49,
                label,
                upd,
                m.note,
                m.manufacturer,
                m.coefficient,
                0,
                0
              )
            }).filter(_.statem_id != 0).filter(_.dir_id != 0).map(specMaterial =>
              DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable] += specMaterial)
            )
            val q = 0
          case Failure(exception) => exception.toString
        }
      case Failure(exception) => exception.toString
    }
  }
}
