package deepsea.time

import akka.actor.Actor
import deepsea.database.MongoCodecs
import deepsea.time.PlanManager._
import io.circe.syntax.EncoderOps

import java.util.Date

object PlanManager{
  case class GetPlan()
  case class GetUserPlan(user: String, from: String)
  case class PlanInterval(id: Long, task_id: Int, user_id: Int, date_start: Long, date_finish: Long, task_type: Int)
  case class AddInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String)
}
class PlanManager extends Actor with PlanManagerHelper with MongoCodecs {
  override def preStart(): Unit = {
//    val now = new Date()
//    var n = nextHour(now.getTime)
//    printDate(n)
//    (1.to(120)).foreach(h => {
//      n = nextHour(n)
//      printDate(n)
//    })
  }
  override def receive: Receive = {
    case GetPlan() =>
      sender() ! getPlan.asJson.noSpaces
    case GetUserPlan(user, from) =>
      sender() ! getUserPlan(user.toIntOption.getOrElse(0), from.toLongOption.getOrElse(0)).asJson
    case AddInterval(taskId, userId, from, hoursAmount, taskType) =>
      addInterval(taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        from.toLongOption.getOrElse(0),
        hoursAmount.toIntOption.getOrElse(0),
        taskType.toIntOption.getOrElse(0))
      sender() ! "success".asJson.noSpaces
    case _ => None
  }
}
