package deepsea.files

import deepsea.App
import deepsea.database.DatabaseManager
import deepsea.files.FileManager.DocumentDirectories
import deepsea.files.classes.FileAttachment
import deepsea.issues.IssueManagerHelper
import deepsea.materials.MaterialManagerHelper
import org.aarboard.nextcloud.api.NextcloudConnector

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait FileManagerHelper extends IssueManagerHelper with MaterialManagerHelper{
  def getDocumentDirectories: List[DocumentDirectories] ={
    DatabaseManager.GetMongoConnection() match {
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
                val sp = "/"
                var path = projectName.cloud + sp + "Documents"
                val paths = List(issue.department, issue.doc_number)
                var pathFull = path
                paths.foreach(p => {
                  pathFull = pathFull + sp + p
                })
                docDirectories.directories.foreach(p => {
                  path = pathFull + sp + p
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

}
