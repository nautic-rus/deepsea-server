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

import java.util.Date
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
              rs.getInt("responsible_id"),
              rs.getString("respons_name"),
              rs.getString("respons_surname"),
              if (rs.getInt("itt") > 0) 1 else 0,
              rs.getString("project_name"),
              suppliers.filter(_.equipm_id == id)
            )
          }
          rs.close()
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
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
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
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
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
      case _ =>
    }
    res.toList
  }
  def getEquipFiles(id: Int): List[EquipFile] = {
    val res = ListBuffer.empty[EquipFile]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from equipments_files where equ_id = $id or $id = 0"
        try {
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += EquipFile(
              rs.getInt("equ_id"),
              rs.getInt("user_id"),
              rs.getString("url"),
              rs.getString("rev"),
              rs.getInt("archived"),
              rs.getLong("create_date"),
              rs.getString("type_name"),
              rs.getInt("id"),
              rs.getLong("archived_date"),
            )
          }
          rs.close()
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
      case _ =>
    }
    res.toList
  }
  def addEquipFile(files: List[EquipFileAdd]): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val d = new Date().getTime
        try {
          files.foreach(file => {
            val query = s"insert into equipments_files values (${file.equ_id}, ${file.user_id}, '${file.url}', '${file.rev}', 0, $d, '${file.type_name}', default, 0)"
            s.execute(query)
          })
          s.close()
          c.close()
          "success"
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
            "error: " + e.toString
        }
      case _ => "error: no database connection"
    }
  }
  def delEquipFile(id: Int): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"delete from equipments_files where equ_id = $id"
        try {
          s.execute(query)
          s.close()
          c.close()
          "success"
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
            "error: " + e.toString
        }
      case _ => "error: no database connection"
    }
  }
  def getSupFiles(id: Int): List[SuppFile] = {
    val res = ListBuffer.empty[SuppFile]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from suppliers_files where supplier_id = $id or $id = 0"
        try {
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += SuppFile(
              rs.getInt("supplier_id"),
              rs.getInt("user_id"),
              rs.getString("url"),
              rs.getString("rev"),
              rs.getInt("archived"),
              rs.getLong("create_date"),
              rs.getString("type_name"),
              rs.getInt("id"),
              rs.getLong("archived_date"),
            )
          }
          rs.close()
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
      case _ =>
    }
    res.toList
  }
  def addSupFile(files: List[SuppFileAdd]): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val d = new Date().getTime
        try {
          files.foreach(file => {
            val query = s"insert into suppliers_files values (${file.supplier_id}, ${file.user_id}, '${file.url}', '${file.rev}', 0, $d, '${file.type_name}', default, 0)"
            s.execute(query)
          })
          s.close()
          c.close()
          "success"
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
            "error: " + e.toString
        }
      case _ => "error: no database connection"
    }}
  def delSupFile(id: Int): List[EquipFile] = {
    val res = ListBuffer.empty[EquipFile]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val d = new Date().getTime
        val query = s"delete from suppliers_files where supplier_id = $id"
        try {
          s.execute(query)
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
      case _ =>
    }
    res.toList
  }
}
