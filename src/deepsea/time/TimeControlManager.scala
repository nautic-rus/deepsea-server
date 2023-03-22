package deepsea.time

import akka.actor.Actor
import com.mongodb.BasicDBObject
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import deepsea.database.DBManager.GetFireBaseConnection
import deepsea.database.{DBManager, DatabaseManager, MongoCodecs}
import deepsea.files.FileManager.{TreeDirectory, treeFileDirectoriesCollection}
import deepsea.materials.MaterialManager.{MaterialNode, MaterialNodeHistory}
import deepsea.time.TimeControlManager.{AddSpyWatch, AddUserWatch, GetSpyWatches, GetTime, GetUserTimeControl, GetUserWatches, SpyWatch, TimeControlInterval, UserWatch}
import org.mongodb.scala.MongoCollection
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.ReplaceOptions

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object TimeControlManager {
  case class GetUserTimeControl(user: String)
  case class AddUserWatch(json: String)
  case class AddSpyWatch(json: String)
  case class GetUserWatches()
  case class GetSpyWatches()
  case class GetTime()
  case class UserWatch(user: String, var activity: Long, image: String)
  case class SpyWatch(user: String, var activity: Long, images: List[String], active: String, activeIndex: Int)

  case class TimeControlInterval(userId: String, startTime: Long, endTime: Long, startDate: Long, endDate: Long, addDoor: Int = -1, closeDoor: Int = -1)
}
class TimeControlManager extends Actor with MongoCodecs{
  val length = 100
  override def receive: Receive = {
    case GetUserTimeControl(user) => sender() ! (getUserTimeControl(user)).asJson.noSpaces
    case AddUserWatch(json) =>
      addUserWatch(json)
      sender() ! "success".asJson.noSpaces
    case AddSpyWatch(json) =>
      addSpyWatch(json)
      sender() ! "success".asJson.noSpaces
    case GetSpyWatches() =>
      sender() ! getSpyWatches.asJson.noSpaces
    case GetUserWatches() =>
      sender() ! getUserWatches.asJson.noSpaces
    case GetTime() =>
      sender() ! getTime.asJson.noSpaces
    case _ => None
  }
  def addSpyWatch(json: String): Unit ={
    decode[SpyWatch](json) match {
      case Right(value) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val spyWatches: MongoCollection[SpyWatch] = mongo.getCollection("spyWatches")
            Await.result(spyWatches.replaceOne(and(equal("user", value.user)), value, org.mongodb.scala.model.ReplaceOptions().upsert(true)).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  def getSpyWatches: List[SpyWatch] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val spyWatches: MongoCollection[SpyWatch] = mongo.getCollection("spyWatches")
        Await.result(spyWatches.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[SpyWatch] =>
            values.toList
          case _ => List.empty[SpyWatch]
        }
      case _ => List.empty[SpyWatch]
    }
  }
  def addUserWatch(json: String): Unit ={
    decode[UserWatch](json) match {
      case Right(value) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val userWatches: MongoCollection[UserWatch] = mongo.getCollection("userWatches")
            Await.result(userWatches.replaceOne(and(equal("user", value.user)), value, org.mongodb.scala.model.ReplaceOptions().upsert(true)).toFuture(), Duration(30, SECONDS))
//
//            Await.result(userWatches.deleteMany(and(equal("user", value.user))).toFuture(), Duration(30, SECONDS))
//            Await.result(userWatches.insertOne(value).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  def getUserWatches: List[UserWatch] ={
    DBManager.GetMongoConnection() match {
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
  def getTime: Long = new Date().getTime
}
