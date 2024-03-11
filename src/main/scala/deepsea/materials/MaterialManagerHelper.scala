package deepsea.materials

import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.database.DBManager.RsIterator
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.materials.MaterialManager._
import deepsea.time.PlanManager.IssuePlan
import org.aarboard.nextcloud.api.NextcloudConnector
import org.mongodb.scala.model.Filters
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait MaterialManagerHelper extends IssueManagerHelper {
  def getNodes: List[MaterialNode] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n-nodes").find[MaterialNode].toFuture(), Duration(30, SECONDS)) match {
          case nodes => nodes.toList
          case _ => List.empty[MaterialNode]
        }
      case _ => List.empty[MaterialNode]
    }
  }
  def getMaterials: List[Material] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n").find[Material]().toFuture(), Duration(30, SECONDS)) match {
          case dbMaterials => dbMaterials.toList
          case _ => List.empty[Material]
        }
      case _ => List.empty[Material]
    }
  }
  def getMaterial(code: String): Option[Material] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n").find[Material](Filters.eq("code", code)).first().toFuture(), Duration(30, SECONDS)) match {
          case material: Material => Option(material)
          case _ => Option.empty[Material]
        }
      case _ => Option.empty[Material]
    }
  }
  def getEquipments(suppliers: List[Supplier]): List[Equipment] = {
    val res = ListBuffer.empty[Equipment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/equipments.sql").mkString
        try {
          val rs = s.executeQuery(query)
          while (rs.next()){
            val id = rs.getInt("id")
            res += Equipment(
              id,
              rs.getInt("sfi"),
              rs.getString("name"),
              rs.getString("descriptions"),
              rs.getString("department"),
              rs.getString("comment"),
              rs.getString("respons_name"),
              rs.getString("respons_surname"),
              rs.getInt("itt"),
              rs.getString("project_name"),
              suppliers.filter(_.equipm_id == id)
            )
          }
          rs.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }

  def getSuppliers: List[Supplier] = {
    val res = ListBuffer.empty[Supplier]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/suppliers.sql").mkString
        try {
          val rs = s.executeQuery(query)
          while (rs.next()){
            res += Supplier(
              rs.getInt("suppliers_id"),
              rs.getInt("equipm_id"),
              rs.getString("description"),
              rs.getString("status")
            )
          }
          rs.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }

  def getSFIs: List[SFI] = {
    val res = ListBuffer.empty[SFI]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = "select * from sfi"
        try {
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += SFI(
              rs.getString("code"),
              rs.getString("ru"),
              rs.getString("en")
            )
          }
          rs.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }
}
