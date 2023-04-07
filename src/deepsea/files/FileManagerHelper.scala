package deepsea.files

import akka.http.scaladsl.model.Uri.Path
import com.sun.mail.iap.ByteArray
import deepsea.App
import deepsea.database.DBManager.RsIterator
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.files.FileManager.{CloudFile, DocumentDirectories}
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.materials.MaterialManagerHelper
import org.aarboard.nextcloud.api.NextcloudConnector
import org.apache.http.client.utils.URIUtils
import org.mongodb.scala.Observable
import org.mongodb.scala.gridfs.GridFSBucket
import play.api.libs.json.Json

import java.io.{BufferedInputStream, File, FileOutputStream, FileWriter, InputStream}
import java.net.{URL, URLEncoder}
import java.nio.charset.Charset
import java.nio.file.{Files, StandardCopyOption}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS, pairIntToDuration, pairLongToDuration}

trait FileManagerHelper extends IssueManagerHelper with MaterialManagerHelper{
  val sp: String = File.pathSeparator
  val spCloud: String = "/"


  def getDocumentFilesAux(id: Int): List[FileAttachment]={
    val res = ListBuffer.empty[FileAttachment]
    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, App.Cloud.UserName, App.Cloud.Password)
    getIssueDetails(id) match {
      case Some(issue) =>
        getProjectNames.find(_.pdsp == issue.project) match {
          case Some(projectName) =>
            getDocumentDirectories.find(x => x.project == projectName.rkd && x.department == issue.department) match {
              case Some(docDirectories) =>
                val pathFull = List(projectName.cloud, "Documents", issue.department, issue.doc_number).mkString(spCloud)

                if (!cloud.folderExists(pathFull)){
                  cloud.listFolderContent(pathFull)
                }

                docDirectories.directories.foreach(p => {
                  val path = pathFull + spCloud + p
                  if (cloud.folderExists(path)){
                    cloud.listFolderContent(path, 1, false, true).toArray.toList.map(_.toString).foreach(file => {
                      val props = cloud.getProperties(file, false)
                      res += new FileAttachment(
                        file.split("/").last,
                        file,
                        props.getModified.getTime,
                        props.getOwnerDisplayName,
                        "",
                        p,
                        1
                      )
                    })
                  }
                })
              case _ =>
            }
          case _ =>
        }
      case _ =>
    }
    res.toList
  }
  def getFileFromCloud(path: String): File ={
    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, App.Cloud.UserName, App.Cloud.Password)
    val dir = Files.createTempDirectory("download")
    if (cloud.fileExists(path)){
      cloud.downloadFile(path, dir.toString)
    }
    new File(Files.list(dir).toArray.toList.head.toString)
  }
  def copyFilesToDirectory(docNumber: String, department: String, attachments: List[FileAttachment], directory: String): String ={
    val project = if (docNumber.contains("-")) docNumber.split("-").head else ""
    getDocumentDirectories.find(_.project == project) match {
      case Some(docDirectories) =>
        val paths = List(directory, "Documents", department, docNumber)
        1.to(paths.length).foreach(p => {
          val path = paths.take(p).mkString(spCloud)
          if (!new File(path).exists()){
            new File(path).mkdir()
          }
        })
        val path = paths.mkString(spCloud)
        docDirectories.directories.map(x => path + spCloud + x).foreach(d => {
          if (!new File(d).exists()){
            new File(d).mkdir()
          }
        })
        val fullPath = paths.mkString(spCloud)
        attachments.foreach(fileUrl => {
          try{
            var name = fileUrl.url.split(spCloud).last
            val enc = fileUrl.url.replace(fileUrl.url.split(spCloud).takeRight(1).head, "") + URLEncoder.encode(fileUrl.url.split(spCloud).takeRight(1).head, "UTF-8")
            var file = new File(List(fullPath, fileUrl.group, name).mkString(spCloud))
            while (file.exists()){
              name = name.replace("\\.", "%\\.")
              file = new File(List(fullPath, fileUrl.group, name).mkString(spCloud))
            }
            val url = new URL(enc)
            val stream = url.openStream()
            Files.copy(stream, file.toPath, StandardCopyOption.REPLACE_EXISTING)
          }
          catch {
            case (e: Exception) =>
              val jk = e
              val jkk = jk
          }
        })
        "success"
      case _ => "ERROR: There is no defined directories for this document"
    }
  }
  def cloneDocumentFilesToCloud(docNumber: String, department: String, attachments: List[FileAttachment]): String ={
    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, App.Cloud.UserName, App.Cloud.Password)
    val project = if (docNumber.contains("-")) docNumber.split("-").head else ""
    getDocumentDirectories.find(_.project == project) match {
      case Some(docDirectories) =>
        getProjectNames.find(_.rkd == project) match {
          case Some(cloudPath) =>
            val paths = List(cloudPath.cloud, "Documents", department, docNumber)
            generateDocumentDirectories(cloud, paths, docDirectories.directories)
            val fullPath = paths.mkString(sp)
            attachments.foreach(fileUrl => {

              try{
                val name = fileUrl.url.split(sp).last
                val enc = fileUrl.url.replace(fileUrl.url.split(sp).takeRight(1).head, "") + URLEncoder.encode(fileUrl.url.split(sp).takeRight(1).head, "UTF-8")
                val file = File.createTempFile("upload-", "")
                val url = new URL(enc)
                val stream = url.openStream()
                Files.copy(stream, file.toPath, StandardCopyOption.REPLACE_EXISTING)
                cloud.uploadFile(file, List(fullPath, fileUrl.group, name).mkString(sp))
              }
              catch {
                case (e: Exception) =>
                  val jk = e
                  val jkk = jk
              }

            })
            "success"
          case _ => s"ERROR: There is no defined cloud path for project $project"
        }
      case _ => "ERROR: There is no defined directories for this document"
    }
  }
  def generateDocumentDirectories(cloud: NextcloudConnector, paths: List[String], docDirectories: List[String]): Unit ={
    1.to(paths.length).foreach(p => {
      val path = paths.take(p).mkString(spCloud)
      if (!cloud.folderExists(path)){
        cloud.createFolder(path)
      }
    })
    val path = paths.mkString(spCloud)
    docDirectories.map(x => path + spCloud + x).foreach(d => {
      if (!cloud.folderExists(d)){
        cloud.createFolder(d)
      }
    })
  }
  def createDocumentDirectory(id: Int): String ={
    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, App.Cloud.UserName, App.Cloud.Password)
    getIssueDetails(id) match {
      case Some(issue) =>
        getProjectNames.find(_.pdsp == issue.project) match {
          case Some(projectName) =>
            getDocumentDirectories.find(x => x.project == projectName.rkd && x.department == issue.department) match {
              case Some(docDirectories) =>
                val paths = List(projectName.cloudRkd, "Documents", issue.department, issue.doc_number)
                val pathFull = paths.mkString(spCloud)
                if (cloud.folderExists(pathFull)){
                  App.Cloud.Protocol + "://" + App.Cloud.Host + "/apps/files/?dir=/" + pathFull
                }
                else{
                  generateDocumentDirectories(cloud, paths, docDirectories.directories)
                  App.Cloud.Protocol + "://" + App.Cloud.Host + "/apps/files/?dir=/" + pathFull
                }
              case _ => "ERROR: There is no defined directories for this document"
            }
          case _ => s"ERROR: There is no defined cloud path for project ${issue.project}"
        }
      case _ => s"ERROR: There is no issue found with id $id"
    }
  }
  def replaceSymbols(input: String): String = {
    input.replaceAll(sp, "-")
  }
  def createMaterialDirectory(project: String, code: String): String = {
    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, App.Cloud.UserName, App.Cloud.Password)
    val nodes = getNodes
    val projectNames = getProjectNames
    projectNames.find(_.rkd == project) match {
      case Some(cloudPath) =>
        val paths = ListBuffer(cloudPath.cloud, "Materials")
        val directories = cloud.listFolderContent(paths.mkString(spCloud), 10, false, true).toArray.map(_.toString).toList
        val jk = directories
        1.to(4).foreach(x => {
          val nodePath = code.substring(0, 3 * x)
          nodes.find(_.data == nodePath) match {
            case Some(value) => paths += value.label
            case _ => None
          }
        })
        getMaterial(code) match {
          case Some(material) =>
            paths += material.name.replaceAll(spCloud, ",") + " (" + code + ")"
            val pathFull = paths.mkString(spCloud)

            if (directories.contains(pathFull + spCloud)){
              App.Cloud.Protocol + "://" + App.Cloud.Host + "/apps/files/?dir=/" + pathFull
            }
            else {
              directories.find(x => x.contains(code)) match {
                case Some(value) => value
                case _ =>
                  (2.to(paths.length)).foreach(p => {
                    val path = paths.take(p).mkString(spCloud)
                    if (!directories.contains(path + spCloud)){
                      cloud.createFolder(path)
                    }
                  })
                  App.Cloud.Protocol + "://" + App.Cloud.Host + "/apps/files/?dir=/" + pathFull
              }
            }
          case _ => s"ERROR: There is no material with code $code"
        }
      case _ => s"ERROR: There is no defined cloud path for project $project"
    }
  }
  def addFileToMongo(fileName: String, stream: InputStream): Unit ={
    DBManager.GetMongoFilesConnection() match {
      case Some(mongo) =>
        val gridFSBucket = GridFSBucket(mongo)
        //gridFSBucket.uploadFromObservable(fileName, stream.)
      case _ => None
    }
  }
}
