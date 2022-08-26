package deepsea.files

import deepsea.database.DatabaseManager
import deepsea.files.FileManager.DocumentDirectories
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait FileManagerHelper {
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
}
