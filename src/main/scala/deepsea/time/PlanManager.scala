package deepsea.time

import akka.actor.Actor
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager
import deepsea.database.MongoCodecs
import deepsea.issues.IssueManager.AssignIssue
import deepsea.time.PlanManager._
import io.circe.syntax.EncoderOps

object PlanManager{
  case class GetPlan()
  case class GetPlanByDays(date: String)
  case class GetUserPlan(user: String, from: String)
  case class PlanInterval(id: Int, task_id: Int, user_id: Int, date_start: Long, date_finish: Long, task_type: Int, hours_amount: Int, consumed: Int)
  case class AddInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String, fromUser: String)
  case class InsertInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String, fromUser: String)
  case class InsertConsumedInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String)
  case class DayInterval(taskId: Int, hours: Int, hours_total: Int, id: Int, date_start: Long, consumed: Int)
  case class PlanByDays(day: Int, month: Int, year: Int, ints: List[DayInterval])
  case class UserPlan(userId: Int, plan: List[PlanByDays])
  case class DeleteInterval(id: String, fromUser: String)
  case class IssuePlan(id: Int, name: String, docNumber: String,
                       plan: Int, status: String, issue_type: String, period: String,
                       assigned_to: String, project: String, department: String,
                       closing_status: String, stage_date: Long,
                       consumed: Int, inPlan: Int, available: Int, available_limit: Int)
  case class GetPlanIssues()
  case class GetPlanIssue(id: String)
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
    case GetPlanByDays(date) =>
      sender() ! getPlanByDays(date.toLongOption.getOrElse(0)).asJson.noSpaces
    case GetUserPlan(user, from) =>
      sender() ! getUserPlan(user.toIntOption.getOrElse(0), from.toLongOption.getOrElse(0)).asJson
    case DeleteInterval(id, fromUser) =>
      deleteInterval(id.toIntOption.getOrElse(0))
      getInterval(id.toIntOption.getOrElse(0)).headOption match {
        case Some(int) =>
          ActorManager.issue ! AssignIssue(int.task_id.toString, "", 0.toString, 0.toString, "Нет", "New", fromUser)
        case _ => None
      }
      sender() ! "success".asJson.noSpaces
    case AddInterval(taskId, userId, from, hoursAmount, taskType, fromUser) =>
      val res = addInterval(taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        from.toLongOption.getOrElse(0),
        hoursAmount.toIntOption.getOrElse(0),
        taskType.toIntOption.getOrElse(0))
      getInterval(res).headOption match {
        case Some(int) =>
          ActorManager.issue ! AssignIssue(int.task_id.toString, userLogin(int.user_id), int.date_start.toString, int.date_finish.toString, "Нет", "AssignedTo", fromUser)
        case _ => None
      }
      val response = res match {
        case -2 => "error: critical code error"
        case -1 => "error: wrong planing date"
        case _ => "success"
      }
      sender() ! response.asJson.noSpaces
    case InsertInterval(taskId, userId, from, hoursAmount, taskType, fromUser) =>
      val res = insertInterval(taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        from.toLongOption.getOrElse(0),
        hoursAmount.toIntOption.getOrElse(0),
        taskType.toIntOption.getOrElse(0))
      getInterval(res).headOption match {
        case Some(int) =>
          ActorManager.issue ! AssignIssue(int.task_id.toString, userLogin(int.user_id), int.date_start.toString, int.date_finish.toString, "Нет", "AssignedTo", fromUser)
        case _ => None
      }
      val response = res match {
        case -2 => "error: critical code error"
        case -1 => "error: wrong planing date"
        case _ => "success"
      }
      sender() ! response.asJson.noSpaces
    case InsertConsumedInterval(taskId, userId, from, hoursAmount, taskType) =>
      val res = insertConsumedInterval(taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        from.toLongOption.getOrElse(0),
        hoursAmount.toIntOption.getOrElse(0),
        taskType.toIntOption.getOrElse(0))
      sender() ! res.asJson.noSpaces
    case GetPlanIssues() =>
      sender() ! getIssues.asJson.noSpaces
    case GetPlanIssue(id) =>
      sender() ! getIssue(id.toIntOption.getOrElse(0)).asJson.noSpaces
    case _ => None
  }
}
