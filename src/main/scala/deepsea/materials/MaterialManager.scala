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


  case class MaterialComplect(id: String, project: String, name: String, materials: List[CMaterial])
  case class CMaterial(material: Material, count: Int)
  case class GetMaterialComplects(project: String)
  case class AddMaterialComplect(project: String, name: String)
  case class RemoveMaterialComplect(id: String)
  case class UpdateMaterialComplect(complectValue: String)


  case class GetEquipments()
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

  case class Equipment(id: Int, sfi: Int, name: String, description: String, department: String, comment: String, responsible_id: Int, respons_name: String, respons_surname: String, itt: Int, project_name: String, suppliers: List[Supplier])
  implicit val EquipmentDecoder: Decoder[Equipment] = deriveDecoder[Equipment]
  implicit val EquipmentEncoder: Encoder[Equipment] = deriveEncoder[Equipment]

  case class EquipmentAdd(id: Int, name: String, description: String, sfi: String, project_id: Int, responsible_id: Int, department_id: Int, comment: String, status_id: Int)
  implicit val EquipmentAddDecoder: Decoder[EquipmentAdd] = deriveDecoder[EquipmentAdd]
  implicit val EquipmentAddEncoder: Encoder[EquipmentAdd] = deriveEncoder[EquipmentAdd]

  case class Supplier(id: Int, user_id: Int, equip_id: Int, name: String, description: String, comment: String, manufacturer: String, status: String, approvement: Long, last_update: Long)
  implicit val SupplierDecoder: Decoder[Supplier] = deriveDecoder[Supplier]
  implicit val SupplierEncoder: Encoder[Supplier] = deriveEncoder[Supplier]

  case class SupplierAdd(id: Int, approvement: Long, comment: String, status_id: Int, equip_id: Int, user_id: Int, name: String, manufacturer: String, description: String)
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


  case class SpecMaterial(code: String, name: String, descr: String, units: String, weight: Double, supplier: String, statem_id: Int, dir_id: Int, user_id: Int, label: String, last_upd: Long, note: String, manufacturer: String)
  class SpecMaterialTable(tag: Tag) extends Table[SpecMaterial](tag, "materials") {
    val code = column[String]("stock_code", O.PrimaryKey)
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
    override def * = (code, name, descr, units, weight, supplier, statem_id, dir_id, user_id, label, last_upd, note, manufacturer) <> ((SpecMaterial.apply _).tupled, SpecMaterial.unapply)
  }
  implicit val SpecMaterialDecoder: Decoder[SpecMaterial] = deriveDecoder[SpecMaterial]
  implicit val SpecMaterialEncoder: Encoder[SpecMaterial] = deriveEncoder[SpecMaterial]


  case class MaterialDirectory(id: Int, name: String, parent_id: Int, user_id: Int, date: Long, old_code: String)
  class MaterialDirectoryTable(tag: Tag) extends Table[MaterialDirectory](tag, "materials_directory") {
    val id = column[Int]("id", O.AutoInc)
    val name = column[String]("name")
    val parent_id = column[Int]("parent_id")
    val user_id = column[Int]("user_id")
    val date = column[Long]("date")
    val old_code = column[String]("old_code")
    override def * = (id, name, parent_id, user_id, date, old_code) <> ((MaterialDirectory.apply _).tupled, MaterialDirectory.unapply)
  }
  implicit val MaterialDirectoryDecoder: Decoder[MaterialDirectory] = deriveDecoder[MaterialDirectory]
  implicit val MaterialDirectoryEncoder: Encoder[MaterialDirectory] = deriveEncoder[MaterialDirectory]

  case class MaterialStatement(id: Int, name: String, project_id: Int, code: String, parent_id: Int)
  class MaterialStatementTable(tag: Tag) extends Table[MaterialStatement](tag, "materials_statements") {
    val id = column[Int]("id", O.AutoInc)
    val name = column[String]("name")
    val project_id = column[Int]("project_id")
    val code = column[String]("code")
    val parent_id = column[Int]("parent_id")
    override def * = (id, name, project_id, code, parent_id) <> ((MaterialStatement.apply _).tupled, MaterialStatement.unapply)
  }
  implicit val MaterialStatementDecoder: Decoder[MaterialStatement] = deriveDecoder[MaterialStatement]
  implicit val MaterialStatementEncoder: Encoder[MaterialStatement] = deriveEncoder[MaterialStatement]


  case class GetSpecMaterials()
  case class GetSpecDirectories()
  case class UpdateMaterials()

  case class SupTaskRelations(id: Int, suppliers_id: Int, task_id: Int)
  class SupTaskRelationsTable(tag: Tag) extends Table[SupTaskRelations](tag, "sup_task_relations") {
    val id = column[Int]("id", O.AutoInc)
    val suppliers_id = column[Int]("suppliers_id")
    val task_id = column[Int]("task_id")
    override def * = (id, suppliers_id, task_id) <> ((SupTaskRelations.apply _).tupled, SupTaskRelations.unapply)
  }
  implicit val SupTaskRelationsDecoder: Decoder[SupTaskRelations] = deriveDecoder[SupTaskRelations]
  implicit val SupTaskRelationsEncoder: Encoder[SupTaskRelations] = deriveEncoder[SupTaskRelations]


  case class SupTaskAdd(jsonValue: String)

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
    self ! UpdateMaterials()
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
          Await.result(mongo.getCollection("material-complects").find[MaterialComplect](equal("project", project)).toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[MaterialComplect]
          }
        case _ => List.empty[WCNumberName]
      }
    case AddMaterialComplect(project, name) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val complects: MongoCollection[MaterialComplect] = mongo.getCollection("material-complects")
          Await.result(complects.insertOne(MaterialComplect(UUID.randomUUID().toString, project, name, List.empty[CMaterial])).toFuture(), Duration(30, SECONDS))
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
                  val query = s"insert into equipments values (default, '${eq.name}', '${eq.description}', ${eq.sfi}, ${eq.project_id}, ${eq.responsible_id}, ${eq.department_id}, $date, '${eq.comment}', ${eq.status_id}) returning id"
                  val rs = stmt.executeQuery(query)
                  while (rs.next()) {
                    eqId = rs.getInt("id")
                  }
                  rs.close()
                }
                else{
                  val query = s"update equipments set name = '${eq.name}', descriptions = '${eq.description}', sfi = ${eq.sfi}, project_id = ${eq.project_id}, responsible_id = ${eq.responsible_id}, department_id = ${eq.department_id}, comment = '${eq.comment}', status_id = ${eq.status_id} where id = ${eq.id}"
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
      val res: String = decode[SupplierAdd](jsonValue) match {
        case Right(sup) =>
          DBManager.GetPGConnection() match {
            case Some(pg) =>
              val stmt = pg.createStatement()
              try {
                var supId = sup.id
                if (sup.id == 0){
                  val query = s"insert into suppliers values (default, ${sup.user_id}, ${sup.equip_id}, '${sup.name}', '${sup.description}', '${sup.comment}', ${sup.status_id}, '${sup.manufacturer}, ${sup.approvement}', $d) returning id"
                  val rs = stmt.executeQuery(query)
                  while (rs.next()) {
                    supId = rs.getInt("id")
                  }
                  rs.close()
                }
                else {
                  val query = s"update suppliers set user_id = ${sup.user_id}, equ_id = ${sup.equip_id}, name = '${sup.name}', description = '${sup.description}', comment = '${sup.comment}', status_id = ${sup.status_id}, manufacturer = '${sup.manufacturer}', approvement = ${sup.approvement}, last_update = $d where id = ${sup.id}"
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
      sender() ! getSpecMaterials.onComplete {
        case Success(value) => value.asJson.noSpaces
        case Failure(exception) => exception.toString
      }
    case GetSpecDirectories() =>
      sender() ! getMaterialDirectories.onComplete {
        case Success(value) => value.asJson.noSpaces
        case Failure(exception) => exception.toString
      }

    case SupTaskAdd(jsonValue) =>
      decode[SupTaskRelations](jsonValue) match {
        case Right(supTask) => supTaskAdd(supTask).onComplete {
          case Success(value) => sender() ! value.asJson.noSpaces
          case Failure(exception) => sender() ! ("error:" + exception.toString)
        }
        case Left(error) =>  sender() ! ("error: wrong post data")
      }
    case _ => None
  }
  def getSpecMaterials: Future[List[SpecMaterial]] = {
    DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].result).map(_.toList)
  }
  def getMaterialDirectories: Future[List[MaterialDirectory]] = {
    DBManager.PostgresSQL.run(TableQuery[MaterialDirectoryTable].result).map(_.toList)
  }
  def getMaterialStatements: Future[List[MaterialStatement]] = {
    DBManager.PostgresSQL.run(TableQuery[MaterialStatementTable].result).map(_.toList)
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
                  DBManager.PostgresSQL.run(table += MaterialDirectory(0, r.label, parent.id, 0, 0, r.data))
                case _ => None

              }
            })
            val qw = 0
          case Failure(exception) => println(exception)
        }


      case Failure(exception) => exception.toString
    }
  }
  def updateMaterials(): Unit = {
    getMaterialDirectories.onComplete{
      case Success(directories) =>
        val origMaterials = getMaterials.filter(_.project == "200101")
        val labels = decode[List[SpecMaterial]](Source.fromResource("materials_label.json").mkString) match {
          case Right(value) => value
          case Left(value) => List.empty[SpecMaterial]
        }
        val upd = new Date().getTime
        getMaterialStatements.onComplete {
          case Success(statements) =>
            origMaterials.map(m => {
              val rootName = directories.sortBy(_.old_code.length).reverse.findLast(x => m.code.contains(x)) match {
                case Some(value) => value.name
                case _ => ""
              }
              val stId = statements.find(x => x.name.contains(rootName)) match {
                case Some(value) => value.id
                case _ => 0
              }
              val dirId = directories.sortBy(_.old_code.length).findLast(x => m.code.contains(x.old_code)) match {
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
                m.manufacturer
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
