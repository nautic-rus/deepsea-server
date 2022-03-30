package deepsea.files

import akka.actor.Actor
import deepsea.App
import deepsea.files.FileManager.{CreateFile, GetPdSpList}
import deepsea.files.classes.FileAttachment
import org.aarboard.nextcloud.api.NextcloudConnector
import org.aarboard.nextcloud.api.filesharing.{SharePermissions, ShareType}
import play.api.libs.json.{JsValue, Json}

import java.io.InputStream
import scala.collection.mutable.ListBuffer

object FileManager{
  case class CreateFile(fileName: String, stream: InputStream, filePath: String, login: String, password: String)
  case class GetPdSpList()
}
class FileManager extends Actor{
  override def receive: Receive = {
    case CreateFile(fileName, stream, filePath, login, password) =>
//      sender() ! Json.toJson(new FileAttachment(fileName, uploadFile(fileName, stream, filePath, login, password)))
    case GetPdSpList() =>
      sender() ! getPDSPList
    case _ => None
  }
  def uploadFile(fileName: String, stream: InputStream, filePath: String, login: String, password: String): String ={
    val nextCloud = new NextcloudConnector("cloud.nautic-rus.ru", true, 443, login, password)
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
    val cloud = new NextcloudConnector("cloud.nautic-rus.ru", true, 443, "nnovikov", "Ship1234")
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
}
