package deepsea.files

import deepsea.App
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.files.FileManager.DocumentDirectories
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.materials.MaterialManagerHelper
import org.aarboard.nextcloud.api.NextcloudConnector
import org.apache.http.client.utils.URIUtils

import java.io.{BufferedInputStream, File, FileOutputStream, FileWriter}
import java.net.{URL, URLEncoder}
import java.nio.charset.Charset
import java.nio.file.{Files, StandardCopyOption}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS, pairIntToDuration, pairLongToDuration}

trait FileManagerHelper extends IssueManagerHelper with MaterialManagerHelper{
  val sp: String = File.pathSeparator

  def getDocumentDirectories: List[DocumentDirectories] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("document-directories").find[DocumentDirectories]().toFuture(), Duration(30, SECONDS)) match {
          case projectNames => projectNames.toList
          case _ => List.empty[DocumentDirectories]
        }
      case _ => List.empty[DocumentDirectories]
    }
  }
  def getDocumentFiles(id: Int): List[FileAttachment]={
    val res = ListBuffer.empty[FileAttachment]
    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, App.Cloud.UserName, App.Cloud.Password)
    getIssueDetails(id) match {
      case Some(issue) =>
        getProjectNames.find(_.pdsp == issue.project) match {
          case Some(projectName) =>
            getDocumentDirectories.find(x => x.project == issue.project && x.department == issue.department) match {
              case Some(docDirectories) =>
                val pathFull = List(projectName.cloud, "Documents", issue.department, issue.doc_number).mkString(sp)

                if (!cloud.folderExists(pathFull)){
                  cloud.listFolderContent(pathFull)
                }

                docDirectories.directories.foreach(p => {
                  val path = pathFull + sp + p
                  if (!cloud.folderExists(path)){
                    cloud.listFolderContent(path).toArray.toList.map(_.toString).foreach(file => {
                      val props = cloud.getProperties(file, false)
                      res += new FileAttachment(
                        file.split("/").last,
                        file,
                        props.getCreation.getTime,
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
    val name = path.split(sp).last
    val dir = Files.createTempDirectory("download")
    val res = new File(List(dir, name).mkString(sp))
    if (cloud.fileExists(path)){
      Files.copy(cloud.downloadFile(path), res.toPath, StandardCopyOption.REPLACE_EXISTING)
    }
    res
  }
  def copyFilesToDirectory(docNumber: String, department: String, attachments: List[FileAttachment], directory: String): String ={
    val project = if (docNumber.contains("-")) docNumber.split("-").head else ""
    getDocumentDirectories.find(_.project == project) match {
      case Some(docDirectories) =>
        val paths = List(directory, "Documents", department, docNumber)
        1.to(paths.length).foreach(p => {
          val path = paths.take(p).mkString(sp)
          if (!new File(path).exists()){
            new File(path).mkdir()
          }
        })
        val path = paths.mkString(sp)
        docDirectories.directories.map(x => path + sp + x).foreach(d => {
          if (!new File(d).exists()){
            new File(d).mkdir()
          }
        })
        val fullPath = paths.mkString(sp)
        attachments.foreach(fileUrl => {
          try{
            var name = fileUrl.url.split(sp).last
            val enc = fileUrl.url.replace(fileUrl.url.split(sp).takeRight(1).head, "") + URLEncoder.encode(fileUrl.url.split(sp).takeRight(1).head, "UTF-8")
            var file = new File(List(fullPath, fileUrl.group, name).mkString(sp))
            while (file.exists()){
              name = name.replace("\\.", "%\\.")
              file = new File(List(fullPath, fileUrl.group, name).mkString(sp))
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
      val path = paths.take(p).mkString(sp)
      if (!cloud.folderExists(path)){
        cloud.createFolder(path)
      }
    })
    val path = paths.mkString(sp)
    docDirectories.map(x => path + sp + x).foreach(d => {
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
            getDocumentDirectories.find(x => x.project == issue.project && x.department == issue.department) match {
              case Some(docDirectories) =>
                val paths = List(projectName.cloud, "Documents", issue.department, issue.doc_number)
                val pathFull = paths.mkString(sp)
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

}
