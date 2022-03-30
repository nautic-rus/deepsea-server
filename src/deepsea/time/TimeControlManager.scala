package deepsea.time

import akka.actor.Actor
import deepsea.database.DatabaseManager.{GetConnection, GetFireBaseConnection}
import deepsea.time.TimeControlManager.{GetUserTimeControl, TimeControlInterval}
import play.api.libs.json.{Json, OWrites}

import java.util.Date
import scala.collection.mutable.ListBuffer

object TimeControlManager {
  case class GetUserTimeControl(user: String)

  case class TimeControlInterval(userId: String, startTime: Long, endTime: Long, startDate: Long, endDate: Long, addDoor: Int = -1, closeDoor: Int = -1)
  implicit val writesTimeControlInterval: OWrites[TimeControlInterval] = Json.writes[TimeControlInterval]
}
class TimeControlManager extends Actor{
  val length = 100
  override def receive: Receive = {
    case GetUserTimeControl(user) => sender() ! Json.toJson(getUserTimeControl(user))
    case _ => None
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
