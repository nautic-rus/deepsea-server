package deepsea.materials

import akka.actor.Actor
import com.mongodb.BasicDBObject
import com.mongodb.client.model.Filters
import deepsea.database.{DBManager, DatabaseManager, MongoCodecs}
import deepsea.materials.MaterialManager._
import io.circe.{jawn, parser}
import org.bson.Document
import play.api.libs.json.{JsValue, Json}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoCollection, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters.{and, equal}
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}
import io.circe.parser._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import io.circe.syntax._
import org.mongodb.scala.result.InsertOneResult

import java.util.{Date, UUID}

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

  case class Supplier(id: Int, user_id: Int, equip_id: Int, name: String, description: String, comment: String, manufacturer: String, status: String, approvement: Int, last_update: Long)
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

  case class RelatedTask(id: Int, issue_id: Int, issue_typ: String, issue_name: String, started_by: String, responsible: String, assigned_to: String, status: String)
  implicit val RelatedTaskDecoder: Decoder[RelatedTask] = deriveDecoder[RelatedTask]
  implicit val RelatedTaskEncoder: Encoder[RelatedTask] = deriveEncoder[RelatedTask]

}
class MaterialManager extends Actor with MongoCodecs with MaterialManagerHelper {

  val collection = "materials-n"
  val collectionNodes = "materials-n-nodes"
  val collectionNodesHistory = "materials-n-nodes-h"
  val collectionHistory = "materials-n-h"

  override def preStart(): Unit = {
    //self ! GetEquipFiles(0)
    //self ! GetEquipments()
//    DatabaseManager.GetMongoConnection() match {
//      case Some(mongo) =>
//        Await.result(mongo.getCollection(collectionNodes).find[MaterialNode](equal("project", "210101")).toFuture(), Duration(30, SECONDS)) match {
//          case nodes =>
//            val nodeCollection: MongoCollection[MaterialNode] = mongo.getCollection(collectionNodes)
//
//            nodes.foreach(n => {
//              val node = n.copy(project = "200101")
//              Await.result(nodeCollection.insertOne(node).toFuture(), Duration(30, SECONDS))
//            })
//
//            sender() ! nodes.toList.asJson.noSpaces
//          case _ => List.empty[MaterialNode]
//        }
//      case _ => List.empty[MaterialNode]
//    }
//    DatabaseManager.GetMongoConnection() match {
//      case Some(mongo) =>
//        Await.result(mongo.getCollection(collection).find[Material](new BasicDBObject("project", "210101")).toFuture(), Duration(30, SECONDS)) match {
//          case dbMaterials =>
//            val errors = ListBuffer.empty[String]
//            dbMaterials.toList.groupBy(_.code).toList.foreach(g => {
//              if (g._2.length > 1){
//                errors += g._1
//              }
//            })
//            val materials: MongoCollection[Material] = mongo.getCollection(collection)
//            dbMaterials.foreach(mat => {
//              val material = mat.copy(id = UUID.randomUUID().toString, project = "200101")
//              Await.result(materials.insertOne(material).toFuture(), Duration(30, SECONDS))
//            })
//            sender() ! dbMaterials.toList.asJson.noSpaces
//          case _ => List.empty[Material]
//        }
//      case _ => List.empty[Material]
//    }
    //self ! GetMaterials("200101")
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
          case Left(value) => "error: wrong post json value"
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
          case Left(value) => "error: wrong post json value"
        }
      }
    case DelSupFile(id) =>
      sender() ! delSupFile(id).asJson.noSpaces
    case GetRelatedTasks(id) =>
      sender() ! getRelatedTasks(id).asJson.noSpaces
    case DelRelatedTask(id) =>
      sender() ! delRelatedTask(id).asJson.noSpaces
    case AddSupplierHistory(jsonValue) =>
      decode[SupplierHistory](jsonValue) match {
        case Right(sup) => sender() ! addSupHistory(sup).asJson.noSpaces
        case Left(value) => "error: wrong post json value"
      }
    case GetSupplierHistory(id) =>
      sender() ! getSupplierHistory(id).asJson.noSpaces
    case _ => None
  }
}
