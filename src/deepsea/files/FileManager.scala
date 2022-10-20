package deepsea.files

import akka.actor.Actor
import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.database.{DatabaseManager, MongoCodecs}
import deepsea.database.DatabaseManager.GetConnection
import deepsea.files.FileManager._
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.issues.classes.IssueMessage
import deepsea.materials.MaterialManager.{Material, MaterialHistory, MaterialNode}
import deepsea.materials.MaterialManagerHelper
import io.circe.parser.{decode, parse}
import io.circe.syntax.EncoderOps
import org.aarboard.nextcloud.api.NextcloudConnector
import org.aarboard.nextcloud.api.filesharing.{SharePermissions, ShareType}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.equal
import play.api.libs.json.{JsValue, Json}

import java.io.{File, FileOutputStream, InputStream}
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object FileManager{
  case class CreateFile(fileName: String, stream: InputStream, filePath: String, login: String, password: String)
  case class GetPdSpList()
  case class CreateMaterialCloudDirectory(project: String, code: String)
  case class CreateDocumentCloudDirectory(id: String)
  case class GetFileFromCloud(path: String)
  case class GetDocumentFiles(id: String)
  case class CloudFile(timeStamp: Long, typeAction: String, user: String, file: String, url: String, id: Int)
  case class GetCloudFiles(filter: String)

  case class TreeFile(url: String, path: String, size: Long, name: String, created_by: String, date: Long)
  case class TreeFileHistory(user: String, date: Long, file: TreeFile)
  case class TreeDirectory(path: String, data: String, name: String, date: Long, created_by: String)

  case class GetTreeFiles()
  case class SetTreeFiles(value: String)
  case class DeleteTreeFiles(values: String, user: String)

  case class GetTreeDirectories()
  case class SetTreeDirectory(value: String)

  case class DocumentDirectories(project: String, department: String, directories: List[String])

  val treeFilesCollection = "tree-files"
  val treeFilesHistoryCollection: String = treeFilesCollection + "-history"

  val treeFileDirectories = "tree-directories"
  val treeFileDirectoriesCollection: String = treeFileDirectories + "-history"
}
class FileManager extends Actor with MongoCodecs with MaterialManagerHelper with FileManagerHelper with IssueManagerHelper {

  override def preStart(): Unit = {
    //createMaterialDirectory("200101", "MTLPIPSTLNON0016")
    //createDocumentDirectory(25)
  }

  override def receive: Receive = {
    case CreateFile(fileName, stream, filePath, login, password) =>

    case GetPdSpList() => sender() ! getPDSPList
    case GetTreeFiles() => sender() ! getTreeFiles.asJson.noSpaces
    case SetTreeFiles(value) =>
      setTreeFiles(value)
      sender() ! "success"
    case DeleteTreeFiles(values, user) =>
      deleteTeeFiles(values, user)
      sender() ! "success"
    case GetTreeDirectories() => getTreeDirectories.asJson.noSpaces
    case SetTreeDirectory(value) =>
      setTreeDirectory(value)
      sender() ! "success"
    case CreateMaterialCloudDirectory(project, code) =>
      sender() ! createMaterialDirectory(project, code).asJson.noSpaces
    case CreateDocumentCloudDirectory(id) =>
      sender() ! createDocumentDirectory(id.toIntOption.getOrElse(0)).asJson.noSpaces
    case GetFileFromCloud(path) =>
      sender() ! getFileFromCloud(path)
    case GetDocumentFiles(id) =>
      sender() ! Json.toJson(getCloudFiles(id.toIntOption.getOrElse(0)))
    case GetCloudFiles(filter) =>
      sender() ! Json.toJson(getCloudFiles(filter))
    case _ => None
  }

  def uploadFile(fileName: String, stream: InputStream, filePath: String, login: String, password: String): String ={
    val nextCloud = new NextcloudConnector("cloud.nautic-rus.com", true, 443, login, password)
    var path = ""
    filePath.split('/').foreach(split => {
      path += split + "/"
      if (!nextCloud.folderExists(path)){
        nextCloud.createFolder(path)
      }
    })
    var fileNameRes = fileName
    while (nextCloud.fileExists(path + "/" + fileNameRes)){
      fileNameRes = fileNameRes.split('.').head + "%" + fileNameRes.split('.').last
    }
    nextCloud.uploadFile(stream, path + "/" + fileNameRes)

    val shareToken = nextCloud.doShare(path + "/" + fileNameRes, ShareType.PUBLIC_LINK, "", false, "", new SharePermissions(0)).getToken
    App.Cloud.Protocol + "://" + App.Cloud.Host + "/s/" + shareToken + "/download/" + fileNameRes
  }
  def getPDSPList: JsValue ={
    val cloud = new NextcloudConnector("cloud.nautic-rus.com", true, 443, "nnovikov", "Ship1234")
    val list = cloud.listFolderContent("ПДСП", 10).toArray.toList.map(x => x.toString)
    val res = ListBuffer.empty[(String, String, String)]
    list.foreach(name => {
      val docName = """(?<=NR002-\d{3}-\d{3}).+""".r.findFirstMatchIn(name) match {
        case Some(value) => value.toString()
        case _ => ""
      }
      val docNumber = """NR002-\d{3}-\d{3}""".r.findFirstMatchIn(name) match {
        case Some(value) => value.toString()
        case _ => ""
      }
      res += new Tuple3[String, String, String](name, docName, docNumber)
    })
   Json.toJson(res)
  }


  def getTreeFiles: List[TreeFile] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection(treeFilesCollection).find[TreeFile].toFuture(), Duration(30, SECONDS)) match {
          case files => files.toList
          case _ => List.empty[TreeFile]
        }
      case _ => List.empty[TreeFile]
    }
  }
  def setTreeFiles(value: String): Unit ={
    decode[List[TreeFile]](value) match {
      case Right(files) =>
        DatabaseManager.GetMongoConnection() match {
          case Some(mongo) =>
            val treeFiles: MongoCollection[TreeFile] = mongo.getCollection(treeFilesCollection)
            Await.result(treeFiles.insertMany(files).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  def deleteTeeFiles(urlsValue: String, user: String): Unit={
    decode[List[String]](urlsValue) match {
      case Right(urls) =>
        DatabaseManager.GetMongoConnection() match {
          case Some(mongo) =>
            val files: MongoCollection[TreeFile] = mongo.getCollection(treeFilesCollection)
            val filesHistory: MongoCollection[TreeFileHistory] = mongo.getCollection(treeFilesHistoryCollection)
            urls.foreach(url => {
              Await.result(mongo.getCollection(treeFilesCollection).find[TreeFile](equal("url", url)).first().toFuture(), Duration(30, SECONDS)) match {
                case oldFile: TreeFile =>
                  Await.result(filesHistory.insertOne(TreeFileHistory(user, new Date().getTime, oldFile)).toFuture(), Duration(30, SECONDS))
                  Await.result(files.deleteOne(equal(treeFilesCollection, oldFile)).toFuture(), Duration(30, SECONDS))
                case _ =>
              }
            })
          case _ =>
        }
      case Left(value) =>
    }
  }
  def getTreeDirectories: List[TreeDirectory] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection(treeFileDirectoriesCollection).find[TreeDirectory].toFuture(), Duration(30, SECONDS)) match {
          case files => files.toList
          case _ => List.empty[TreeDirectory]
        }
      case _ => List.empty[TreeDirectory]
    }
  }
  def setTreeDirectory(jsValue: String): Unit ={
    decode[TreeDirectory](jsValue) match {
      case Right(value) =>
        DatabaseManager.GetMongoConnection() match {
          case Some(mongo) =>
            val files: MongoCollection[TreeDirectory] = mongo.getCollection(treeFileDirectoriesCollection)
            Await.result(files.insertOne(value).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
}
