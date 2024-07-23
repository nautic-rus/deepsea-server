package deepsea.materials

import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.dbase.DBManager.RsIterator
import deepsea.dbase.{DBManager, DatabaseManager}
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.materials.MaterialManager._
import deepsea.time.PlanManager.IssuePlan
import org.aarboard.nextcloud.api.NextcloudConnector
import org.mongodb.scala.model.Filters
import slick.lifted.TableQuery

import scala.language.postfixOps
import slick.jdbc.PostgresProfile.api._
import slick.lifted.{ProvenShape, TableQuery}

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
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
              rs.getString("sfi"),
              rs.getString("name"),
              rs.getString("descriptions"),
              rs.getString("department"),
              rs.getString("comment"),
              rs.getInt("responsible_id"),
              rs.getString("respons_name"),
              rs.getString("respons_surname"),
              if (rs.getInt("itt") > 0) 1 else 0,
              rs.getString("project_name"),
              rs.getString("sfi_unit"),
              rs.getInt("parent_id"),
              rs.getLong("create_date"),
              suppliers.filter(_.equip_id == id)
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
  def getEquipment(id: Int, suppliers: List[Supplier]): List[Equipment] = {
    val res = ListBuffer.empty[Equipment]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/equipments.sql").mkString + " and eq.id = " + id
        try {
          val rs = s.executeQuery(query)
          while (rs.next()){
            val id = rs.getInt("id")
            res += Equipment(
              id,
              rs.getString("sfi"),
              rs.getString("name"),
              rs.getString("descriptions"),
              rs.getString("department"),
              rs.getString("comment"),
              rs.getInt("responsible_id"),
              rs.getString("respons_name"),
              rs.getString("respons_surname"),
              if (rs.getInt("itt") > 0) 1 else 0,
              rs.getString("project_name"),
              rs.getString("sfi_unit"),
              rs.getInt("parent_id"),
              rs.getLong("create_date"),
              suppliers.filter(_.equip_id == id)
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
              rs.getInt("user_id"),
              rs.getInt("equip_id"),
              rs.getString("sup_name"),
              rs.getInt("sup_id"),
              rs.getString("equip_desc"),
              rs.getString("equip_comment"),
              rs.getString("equip_manufacturer"),
              rs.getString("status"),
              rs.getLong("approvement"),
              rs.getLong("last_update"),
              rs.getString("model"),
              rs.getString("ele_param"),
              rs.getString("mech_param"),
              rs.getDouble("weight"),
              rs.getInt("status_id"),
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
    println("getSFIS");
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
        val query = s"select * from equipments_files where (equ_id = $id or $id = 0) and archived = 0"
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
        val d = new Date().getTime
        val query = s"update equipments_files set archived = 1, archived_date = $d where id = $id"
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
        val query = s"select * from suppliers_files where (supplier_id = $id or $id = 0)"
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
        val query = s"update suppliers_files set archived = 1, archived_date = $d where id = $id"
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
  def addSupHistory(h: SupplierHistoryAdd): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val d = new Date().getTime
        try {
          val query = s"insert into suppliers_history values (${h.supplier_id}, ${h.user_id}, '${h.value}', '${h.old_value}', '${h.new_value}', $d, default)"
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
  def getSupplierHistory(id: Int): List[SupplierHistory] = {
    val res = ListBuffer.empty[SupplierHistory]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val d = new Date().getTime
        val query = s"select * from suppliers_history where supplier_id = $id"
        try {
          val rs = s.executeQuery(query)
          while (rs.next()){
            res += SupplierHistory(
              rs.getString("value"),
              rs.getString("old_value"),
              rs.getString("new_value"),
              rs.getInt("user_id"),
              rs.getInt("supplier_id"),
              rs.getLong("date"),
            )
          }
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
  def getRelatedTasks(id: Int): List[RelatedTask] = {
    val res = ListBuffer.empty[RelatedTask]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/related_tasks.sql").mkString.replace("&supplier_id", id.toString)
        try {
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += RelatedTask(
              rs.getInt("id"),
              rs.getInt("issue_id"),
              rs.getString("issue_type"),
              rs.getString("issue_name"),
              rs.getString("started_by"),
              rs.getString("responsible"),
              rs.getString("assigned_to"),
              rs.getString("status"),
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
  def delRelatedTask(id: Int): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"delete from sup_task_relations where id = $id"
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
      case _ => "error: no connection with database"
    }
  }

  def supTaskAdd(supTask: SupTaskRelations): Future[Int] = {
    val table = TableQuery[SupTaskRelationsTable]
    DBManager.PostgresSQL.run((table returning (table.map(_.id))) += supTask)
  }
  def addSupName(supName: SupName): Future[Int] = {
    val table = TableQuery[SupNameTable]
    DBManager.PostgresSQL.run((table returning (table.map(_.id))) += supName)
  }

  def getEqSupMatRelations(supId: String): List[EqSupMatRelations] = {
    val res = ListBuffer.empty[EqSupMatRelations]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/supmatrelations.sql").mkString.replace("&supId", supId)
        try {
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += EqSupMatRelations(
              rs.getInt("materials_id"),
              rs.getString("name"),
              rs.getString("stock_code"),
              Option(rs.getString("doc_number")).getOrElse(""),
              Option(rs.getInt("issue_id")).getOrElse(0),
              rs.getInt("equ_id"),
              rs.getString("dep_name"),
              rs.getString("foran")
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

}
