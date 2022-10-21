package deepsea.materials

import akka.actor.Actor
import com.mongodb.BasicDBObject
import com.mongodb.client.model.Filters
import deepsea.database.{DatabaseManager, MongoCodecs}
import deepsea.database.DatabaseManager.GetMongoConnection
import deepsea.materials.MaterialManager.{GetMaterialNodes, GetMaterials, GetWCDrawings, GetWCZones, GetWeightControl, Material, MaterialHistory, MaterialNode, MaterialNodeHistory, SetWeightControl, UpdateMaterial, UpdateMaterialNode, WCNumberName, WeightControl}
import io.circe.{jawn, parser}
import org.bson.Document
import play.api.libs.json.{JsValue, Json}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoCollection, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters.equal

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
  case class UpdateMaterial(material: String, user: String, remove: String = "0")
  case class GetMaterialNodes()
  case class UpdateMaterialNode(data: String, label: String, user: String, remove: String = "0")
  case class GetWeightControl()
  case class GetWCDrawings()
  case class GetWCZones()
  case class SetWeightControl(controlValue: String)
  case class MaterialHistory(material: Material, user: String, date: Long = new Date().getTime)
  case class Material(
                       name: String = "",
                       description: String = "",
                       category: String = "",
                       code: String = "",
                       units: String = "",
                       singleWeight: Double = 0,
                       projects: List[String] = List.empty[String],
                       document: String = "",
                       provider: String = "",
                       note: String = "",
                       comment: String = "",
                       manufacturer: String = "",
                       coefficient: Double = 1,
                       id: String = UUID.randomUUID().toString,
                       translations: List[MaterialTranslation] = List.empty[MaterialTranslation],
                       itt: Int = 0,
                       approved: Int = 0) {
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
  case class ProjectName(id: String, rkd: String, pdsp: String, foran: String, cloud: String)
  case class MaterialTranslation(lang: String, name: String, description: String)
  case class MaterialNode(label: String, data: String, user: String, date: Long)
  case class MaterialNodeHistory(node: MaterialNode, user: String, date: Long = new Date().getTime)

  case class WeightControl(docNumber: String, docName: String, zoneNumber: String, moveElement: String, zoneName: String, mount: Int, side: Int, weight: Double, x: Double, y: Double, z: Double, user: String, date: Long, project: String)
  case class WCNumberName(number: String, name: String, project: String)
}
class MaterialManager extends Actor with MongoCodecs{

  val collection = "materials-n"
  val collectionNodes = "materials-n-nodes"
  val collectionNodesHistory = "materials-n-nodes-h"
  val collectionHistory = "materials-n-h"

  override def preStart(): Unit = {
    self ! GetMaterials("200101")
  }
  override def receive: Receive = {
    case GetMaterialNodes() =>
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection(collectionNodes).find[MaterialNode].toFuture(), Duration(30, SECONDS)) match {
            case nodes => sender() ! nodes.toList.asJson.noSpaces
            case _ => List.empty[MaterialNode]
          }
        case _ => List.empty[MaterialNode]
      }
    case UpdateMaterialNode(data, label, user, remove) =>
      val node = MaterialNode(label, data, user, new Date().getTime)
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          val nodes: MongoCollection[MaterialNode] = mongo.getCollection(collectionNodes)
          val nodesHistory: MongoCollection[MaterialNodeHistory] = mongo.getCollection(collectionNodesHistory)
          Await.result(nodes.find(new BasicDBObject("data", node.data)).first().toFuture(), Duration(30, SECONDS)) match {
            case oldValue: MaterialNode =>
              Await.result(nodesHistory.insertOne(MaterialNodeHistory(oldValue, user)).toFuture(), Duration(30, SECONDS))
              Await.result(nodes.deleteOne(new BasicDBObject("data", node.data)).toFuture(), Duration(30, SECONDS))
            case _ =>
          }
          if (remove == "0"){
            Await.result(nodes.insertOne(node).toFuture(), Duration(30, SECONDS))
          }
        case _ =>
      }
      sender() ! Json.toJson("success")
    case GetMaterials(project) =>
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection(collection).find[Material](new BasicDBObject("projects", project)).toFuture(), Duration(30, SECONDS)) match {
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
    case UpdateMaterial(materialValue, user, remove) =>
      decode[Material](materialValue) match {
        case Right(material) =>
          DatabaseManager.GetMongoConnection() match {
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
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("weightControl").find[WeightControl]().toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[WeightControl]
          }
        case _ => List.empty[WeightControl]
      }
    case SetWeightControl(controlValue) =>
      decode[WeightControl](controlValue) match {
        case Right(weightControl) =>
          DatabaseManager.GetMongoConnection() match {
            case Some(mongo) =>
              val controls: MongoCollection[WeightControl] = mongo.getCollection("weightControl")
              Await.result(controls.insertOne(weightControl).toFuture(), Duration(30, SECONDS))
            case _ =>
          }
        case Left(value) =>
      }
      sender() ! Json.toJson("success")
    case GetWCDrawings() =>
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("wcDrawings").find[WCNumberName]().toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[WCNumberName]
          }
        case _ => List.empty[WCNumberName]
      }
    case GetWCZones() =>
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection("wcZones").find[WCNumberName]().toFuture(), Duration(30, SECONDS)) match {
            case dbValues => sender() ! dbValues.toList.asJson.noSpaces
            case _ => List.empty[WCNumberName]
          }
        case _ => List.empty[WCNumberName]
      }
//    case GetMaterials(project) =>
//      GetMongoConnection() match {
//        case Some(connection) =>
//          val res = ListBuffer.empty[JsValue]
//          val col = connection.getCollection("materials").find(Filters.and(Filters.eq("project", project), Filters.eq("removed", 0))).map(_.toJson).iterator()
//          while (col.hasNext){
//            res += Json.parse(col.next())
//          }
//          sender() ! Json.toJson(res)
//        case _ => None
//      }
//    case UpdateMaterial(material) =>
//      GetMongoConnection() match {
//        case Some(connection) =>
//          val doc = Document.parse(Json.toJson(material).toString())
//          val filters = Filters.and(Filters.eq("id", material.id),  Filters.eq("removed", 0))
//          if (connection.getCollection("materials").find(filters).map(_.toJson).iterator().hasNext) {
//            connection.getCollection("materials").replaceOne(filters, doc)
//          } else {
//            connection.getCollection("materials").insertOne(doc)
//          }
//          sender() ! "success"
//        case _ => None
//      }
    case _ => None
  }
}
