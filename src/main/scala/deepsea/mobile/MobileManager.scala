package deepsea.mobile

import akka.actor.Actor
import com.mongodb.BasicDBObject
import deepsea.dbase.{DBManager, DatabaseManager, MongoCodecs}
import deepsea.materials.MaterialManager.{GetMaterials, Material}
import deepsea.mobile.MobileManager.{Drawing, DrawingInfo, GetDrawingInfo, GetDrawings, OrizInfo}
import io.circe.syntax.EncoderOps
import org.mongodb.scala.{Document, documentToUntypedDocument}
import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object MobileManager{

  case class GetDrawings()
  case class GetDrawingInfo(drawingName: String)

  case class OrizInfo(oriz: String, name: String, user: String, date: Long, status: String, dateSend: String, dateApproved: String)
  case class DrawingInfo(name: String, docNumber: String, docName: String, revision: String, user: String, date: Long, oriz: List[OrizInfo])
  case class Drawing(name: String, descr: String, rev: String, user: String, date: Long)
}
class MobileManager extends Actor with MongoCodecs{
  override def receive: Receive = {
    case GetDrawings() =>
      sender() ! Json.toJson(getDrawings)
    case GetDrawingInfo(drawingName) =>
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val oriz = Await.result(mongo.getCollection("orIzDrawings").find(new BasicDBObject("name", drawingName)).map(doc => {
              OrizInfo(
                doc.getString("oriz"),
                doc.getString("name"),
                doc.getString("user"),
                doc.get("date") match {
                  case Some(value) => value.asNumber().longValue()
                  case _ => 0
                },
                doc.get("status") match {
                  case Some(value) => value.asString().getValue
                  case _ => ""
                },
                doc.get("dateSend") match {
                  case Some(value) => value.asString().getValue
                  case _ => ""
                },
                doc.get("dateApproved") match {
                  case Some(value) => value.asString().getValue
                  case _ => ""
                }
            )
          }).toFuture(), Duration(30, SECONDS)) match {
            case dbValues => dbValues.toList
            case _ => List.empty[OrizInfo]
          }
          var drawings = Await.result(mongo.getCollection("drawings").find[Drawing](new BasicDBObject("name", drawingName)).toFuture(), Duration(30, SECONDS)) match {
            case dbValues => dbValues.toList
            case _ => List.empty[Drawing]
          }
          if (drawings.isEmpty){
            drawings = Await.result(mongo.getCollection("drawings").find[Drawing](new BasicDBObject("name", drawingName + "СБ")).toFuture(), Duration(30, SECONDS)) match {
              case dbValues => dbValues.toList
              case _ => List.empty[Drawing]
            }
          }

          if (drawings.nonEmpty){
            val drawing = drawings.head
            sender() ! DrawingInfo(
              drawingName,
              drawing.name,
              drawing.descr,
              drawing.rev,
              drawing.user,
              drawing.date,
              oriz
            ).asJson.noSpaces
          }
          else{
            sender() ! "not found"
          }

        case _ => sender() ! "not found"
      }
    case _ => None
  }


  def getDrawings: List[String] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("drawings").find[Document].toFuture(), Duration(30, SECONDS)) match {
          case files => files.toList.map(x => x.getString("name"))
          case _ => List.empty[String]
        }
      case _ => List.empty[String]
    }
  }
}
