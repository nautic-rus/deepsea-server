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

  case class Equipment(id: Int, sfi: Int, name: String, department: String, respons_name: String, respons_surname: String, itt: Int, suppliers: List[Supplier])
  implicit val EquipmentDecoder: Decoder[Equipment] = deriveDecoder[Equipment]
  implicit val EquipmentEncoder: Encoder[Equipment] = deriveEncoder[Equipment]

  case class Supplier(suppliers_id: Int, equipm_id: Int, description: String, status: String)
  implicit val SupplierDecoder: Decoder[Supplier] = deriveDecoder[Supplier]
  implicit val SupplierEncoder: Encoder[Supplier] = deriveEncoder[Supplier]
}
class MaterialManager extends Actor with MongoCodecs with MaterialManagerHelper {

  val collection = "materials-n"
  val collectionNodes = "materials-n-nodes"
  val collectionNodesHistory = "materials-n-nodes-h"
  val collectionHistory = "materials-n-h"

  override def preStart(): Unit = {
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
    case _ => None
  }
}
