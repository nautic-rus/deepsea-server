package deepsea.time

import akka.actor.Actor
import com.mongodb.BasicDBObject
import deepsea.database.DatabaseManager.{GetConnection, GetFireBaseConnection}
import deepsea.database.{DatabaseManager, MongoCodecs}
import deepsea.files.FileManager.{TreeDirectory, treeFileDirectoriesCollection}
import deepsea.materials.MaterialManager.{MaterialNode, MaterialNodeHistory}
import deepsea.time.TimeControlManager.{AddUserWatch, GetUserTimeControl, GetUserWatches, TimeControlInterval, UserWatch}
import org.mongodb.scala.MongoCollection
import play.api.libs.json.{Json, OWrites, __}
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.mongodb.scala.model.Filters._

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object TimeControlManager {
  case class GetUserTimeControl(user: String)
  case class AddUserWatch(json: String)
  case class GetUserWatches()

  case class UserWatch(user: String, activity: Long, image: String)

  case class TimeControlInterval(userId: String, startTime: Long, endTime: Long, startDate: Long, endDate: Long, addDoor: Int = -1, closeDoor: Int = -1)
  implicit val writesTimeControlInterval: OWrites[TimeControlInterval] = Json.writes[TimeControlInterval]
}
class TimeControlManager extends Actor with MongoCodecs{
  val length = 100
  override def receive: Receive = {
    case GetUserTimeControl(user) => sender() ! Json.toJson(getUserTimeControl(user))
    case AddUserWatch(json) =>
      addUserWatch(json)
      sender() ! "success".asJson.noSpaces
    case GetUserWatches() =>
      sender() ! getUserWatches.asJson.noSpaces
    case _ => None
  }
  def addUserWatch(json: String): Unit ={
    decode[UserWatch](json) match {
      case Right(value) =>
        DatabaseManager.GetMongoConnection() match {
          case Some(mongo) =>
            val userWatches: MongoCollection[UserWatch] = mongo.getCollection("userWatches")
            Await.result(userWatches.insertOne(value).toFuture(), Duration(30, SECONDS))
            Await.result(userWatches.deleteMany(and(equal("user", value.user), notEqual("activity", value.activity))).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  def getUserWatches: List[UserWatch] ={
    DatabaseManager.GetMongoConnection() match {
      case Some(mongo) =>
        val userWatches: MongoCollection[UserWatch] = mongo.getCollection("userWatches")
        Await.result(userWatches.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[UserWatch] =>
            values.toList
          case _ => List.empty[UserWatch]
        }
      case _ => List.empty[UserWatch]
    }
  }
  def getUserTimeControl(user: String): ListBuffer[TimeControlInterval] ={
    val res = ListBuffer.empty[TimeControlInterval]
    GetFireBaseConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from GRAPH_FACT WHERE STARTDATE <= current_date and STARTDATE >= current_date - $length and uid = '$user'"
        val rs = s.executeQuery(query)
        while (rs.next()){
          res += TimeControlInterval(
            rs.getString("UID") match {
              case userId: String => userId
              case _ => ""
            },
            rs.getDate("STARTTIME") match {
              case date: Date => date.getTime
              case _ => 0
            },
            rs.getDate("ENDTIME") match {
              case date: Date => date.getTime
              case _ => 0
            },
            rs.getDate("STARTDATE") match {
              case date: Date => date.getTime
              case _ => 0
            },
            rs.getDate("ENDDATE") match {
              case date: Date => date.getTime
              case _ => 0
            },
            rs.getInt("ADDDOOR") match {
              case value: Int => value
              case _ => 0
            },
            rs.getInt("CLOSEDOOR") match {
              case value: Int => value
              case _ => 0
            }
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res
  }
}
