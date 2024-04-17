package deepsea.storage

import deepsea.dbase.DBManager
import deepsea.materials.MaterialManager.MaterialStatement
import deepsea.storage.StorageManager.{StorageFile, StorageFileTable, StorageUnit, StorageUnitTable}
import io.circe.syntax.EncoderOps
import slick.lifted.TableQuery
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}


trait StorageHelper {
  def updateStorageUnit(storageUnit: StorageUnit): Unit = {
    DBManager.PostgresSQL.run(TableQuery[StorageUnitTable].insertOrUpdate(storageUnit))
  }
  def updateStorageFile(storageFile: StorageFile): Unit = {
    DBManager.PostgresSQL.run(TableQuery[StorageFileTable].insertOrUpdate(storageFile))
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
