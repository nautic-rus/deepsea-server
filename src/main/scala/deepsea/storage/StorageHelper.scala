package deepsea.storage

import deepsea.dbase.DBManager
import deepsea.materials.MaterialManager.{MaterialStatement, SupTaskRelationsTable}
import deepsea.storage.StorageManager.{StorageFile, StorageFileTable, StorageUnit, StorageUnitTable}
import io.circe.syntax.EncoderOps
import slick.lifted.TableQuery
import slick.jdbc.PostgresProfile.api._

import java.util.Date
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}


trait StorageHelper {
  def updateStorageUnit(storageUnit: StorageUnit): Unit = {
    Await.result(DBManager.PostgresSQL.run(TableQuery[StorageUnitTable].insertOrUpdate(storageUnit)), Duration(5, SECONDS))
  }
  def newStorageUnit: StorageUnit = {
    val sUnit = StorageUnit(
      0,
      "Наименование оборудования",
      "",
      "###-###",
      "",
      "",
      "Создан",
      0,
      new Date().getTime,
      0,
      "",
      "",
      0,
      1
    )
    val table = TableQuery[StorageUnitTable]

    val sUnitId = Await.result(DBManager.PostgresSQL.run((table returning (table.map(_.id))) += sUnit), Duration(5, SECONDS)) match {
      case value: Int => value
      case _ => 0
    }

    sUnit.copy(id = sUnitId)
  }
  def updateStorageFile(storageFile: StorageFile): Unit = {
    Await.result(DBManager.PostgresSQL.run(TableQuery[StorageFileTable].insertOrUpdate(storageFile.copy(date_created = new Date().getTime))), Duration(5, SECONDS))
  }
  def getStorageUnits: List[StorageUnit] = {
    Await.result(DBManager.PostgresSQL.run(TableQuery[StorageUnitTable].result.map(_.toList)), Duration(5, SECONDS)) match {
      case response: List[StorageUnit] => response
      case _ => List.empty[StorageUnit]
    }
  }
  def getStorageFiles: List[StorageFile] = {
    Await.result(DBManager.PostgresSQL.run(TableQuery[StorageFileTable].result.map(_.toList)), Duration(5, SECONDS)) match {
      case response: List[StorageFile] => response
      case _ => List.empty[StorageFile]
    }
  }
}
