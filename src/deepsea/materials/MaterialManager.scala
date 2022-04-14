package deepsea.materials

import akka.actor.Actor
import com.mongodb.BasicDBObject
import com.mongodb.client.model.Filters
import deepsea.database.DatabaseManager
import deepsea.database.DatabaseManager.GetMongoConnection
import deepsea.materials.MaterialManager.{GetMaterialNodes, GetMaterials, Material, MaterialHistory, MaterialNode, UpdateMaterial}
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
                       coefficient: Double = 1,
                       id: String = UUID.randomUUID().toString)
  case class MaterialNode(label: String = "", data: String = "")
}
class MaterialManager extends Actor{

  val collection = "materials-n"
  val collectionNodes = "materials-n-nodes"
  val collectionHistory = "materials-n-h"

  override def preStart(): Unit = {
    self ! GetMaterials("P701")
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
    case GetMaterials(project) =>
      DatabaseManager.GetMongoConnection() match {
        case Some(mongo) =>
          Await.result(mongo.getCollection(collection).find[Material](new BasicDBObject("projects", project)).toFuture(), Duration(30, SECONDS)) match {
            case dbMaterials => sender() ! dbMaterials.toList.asJson.noSpaces
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
