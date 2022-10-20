package deepsea.materials

import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.materials.MaterialManager.{Material, MaterialNode, ProjectName}
import org.aarboard.nextcloud.api.NextcloudConnector
import org.mongodb.scala.model.Filters

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait MaterialManagerHelper extends IssueManagerHelper {
  def getNodes: List[MaterialNode] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n-nodes").find[MaterialNode].toFuture(), Duration(30, SECONDS)) match {
          case nodes => nodes.toList
          case _ => List.empty[MaterialNode]
        }
      case _ => List.empty[MaterialNode]
    }
  }
  def getMaterials: List[Material] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n").find[Material]().toFuture(), Duration(30, SECONDS)) match {
          case dbMaterials => dbMaterials.toList
          case _ => List.empty[Material]
        }
      case _ => List.empty[Material]
    }
  }
  def getMaterial(code: String): Option[Material] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n").find[Material](Filters.eq("code", code)).first().toFuture(), Duration(30, SECONDS)) match {
          case material: Material => Option(material)
          case _ => Option.empty[Material]
        }
      case _ => Option.empty[Material]
    }
  }
}
