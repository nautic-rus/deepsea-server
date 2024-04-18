package deepsea.storage

import akka.actor.Actor
import deepsea.dbase.DBManager
import deepsea.storage.StorageManager._
import io.circe.{Decoder, Encoder, jawn}
import io.circe.syntax.EncoderOps
import slick.jdbc.PostgresProfile.api._
import slick.lifted.{ProvenShape, TableQuery}
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}


object StorageManager{
  case class GetStorageUnits()
  case class UpdateStorageUnit(json: String)
  case class GetStorageFiles()
  case class UpdateStorageFile(json: String)
  case class StorageUnit(id: Int, name: String, descr: String, code: String, order: String, supplier: String,
                         status: String, user: Int, date_created: Long, date_supply: Long, pack_list: String, comment: String, removed: Int)
  class StorageUnitTable(tag: Tag) extends Table[StorageUnit](tag, "storage_unit") {
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val name = column[String]("name")
    val descr = column[String]("descr")
    val code = column[String]("code")
    val order = column[String]("order")
    val supplier = column[String]("supplier")
    val status = column[String]("status")
    val user = column[Int]("user")
    val date_created = column[Long]("date_created")
    val date_supply = column[Long]("date_supply")
    val pack_list = column[String]("pack_list")
    val comment = column[String]("comment")
    val removed = column[Int]("removed", O.Default(0))
    override def * = (id, name, descr, code, order, supplier, status, user, date_created, date_supply, pack_list, comment, removed) <> ((StorageUnit.apply _).tupled, StorageUnit.unapply)
  }
  implicit val StorageUnitDecoder: Decoder[StorageUnit] = deriveDecoder[StorageUnit]
  implicit val StorageUnitEncoder: Encoder[StorageUnit] = deriveEncoder[StorageUnit]

  case class StorageFile(id: Int, name: String, url: String, kind: String, unit_id: Int, removed: Int, date_created: Long)
  case class StorageFileTable(tag: Tag) extends Table[StorageFile](tag, "storage_file") {
    val id = column[Int]("id", O.AutoInc, O.PrimaryKey)
    val name = column[String]("name")
    val url = column[String]("url")
    val kind = column[String]("kind")
    val unit_id = column[Int]("unit_id")
    val removed = column[Int]("removed")
    val date_created = column[Long]("date_created")
    override def * = (id, name, url, kind, unit_id, removed, date_created) <> ((StorageFile.apply _).tupled, StorageFile.unapply)
  }
  implicit val StorageFileDecoder: Decoder[StorageFile] = deriveDecoder[StorageFile]
  implicit val StorageFileEncoder: Encoder[StorageFile] = deriveEncoder[StorageFile]


}
class StorageManager extends Actor with StorageHelper {
  override def preStart(): Unit = {
    DBManager.PostgresSQL.run(TableQuery[StorageUnitTable].schema.createIfNotExists)
    DBManager.PostgresSQL.run(TableQuery[StorageFileTable].schema.createIfNotExists)
  }
  override def receive: Receive = {
    case GetStorageUnits() => sender() ! getStorageUnits.asJson.noSpaces
    case GetStorageFiles() => sender() ! getStorageFiles.asJson.noSpaces
    case UpdateStorageUnit(json) =>
      sender() ! (decode[StorageUnit](json) match {
        case Right(value) => updateStorageUnit(value).asJson.noSpaces
        case Left(value) => "error: wrong post json value".asJson.noSpaces
      })
    case UpdateStorageFile(json) =>
      sender() ! (decode[StorageFile](json) match {
        case Right(value) => updateStorageFile(value).asJson.noSpaces
        case Left(value) => "error: wrong post json value".asJson.noSpaces
      })
    case _ => None
  }
}
