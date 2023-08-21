package deepsea.time

import akka.actor.Actor
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager
import deepsea.database.MongoCodecs
import deepsea.issues.IssueManager.AssignIssue
import deepsea.issues.classes.IssueHistory
import deepsea.time.PlanManager._
import io.circe.syntax.EncoderOps

import java.util.Date

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
  case class DeletePausedInterval(id: Int)
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
      getInterval(id.toIntOption.getOrElse(0)).headOption match {
        case Some(int) =>
          if (int.task_id > 0) {
            ActorManager.issue ! AssignIssue(int.task_id.toString, "", 0.toString, 0.toString, "Нет", "New", fromUser, "1")
          }
        case _ => None
      }
      deleteInterval(id.toIntOption.getOrElse(0))
      sender() ! "success".asJson.noSpaces
    case AddInterval(taskId, userId, from, hoursAmount, taskType, fromUser) =>
      val res = addInterval(taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        from.toLongOption.getOrElse(0),
        hoursAmount.toIntOption.getOrElse(0),
        taskType.toIntOption.getOrElse(0))
      if (taskId.toIntOption.getOrElse(0) > 0){
        getInterval(res).headOption match {
          case Some(int) =>
            val intervals = getTaskPlan(int.task_id)
            var dateStart = int.date_start
            var dateFinish = int.date_finish
            if (intervals.nonEmpty) {
              intervals.find(_.date_start < dateStart) match {
                case Some(value) => dateStart = value.date_start
                case _ => None
              }
              intervals.find(_.date_finish > dateFinish) match {
                case Some(value) => dateFinish = value.date_finish
                case _ => None
              }
            }
            ActorManager.issue ! AssignIssue(int.task_id.toString, userLogin(int.user_id), dateStart.toString, dateFinish.toString, "Нет", "AssignedTo", fromUser)
          case _ => None
        }
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
      if (taskId.toIntOption.getOrElse(0) > 0) {
        getInterval(res).headOption match {
          case Some(int) =>
            val intervals = getTaskPlan(int.task_id)
            var dateStart = int.date_start
            var dateFinish = int.date_finish
            if (intervals.nonEmpty) {
              intervals.find(_.date_start < dateStart) match {
                case Some(value) => dateStart = value.date_start
                case _ => None
              }
              intervals.find(_.date_finish > dateFinish) match {
                case Some(value) => dateFinish = value.date_finish
                case _ => None
              }
            }
            ActorManager.issue ! AssignIssue(int.task_id.toString, userLogin(int.user_id), dateStart.toString, dateFinish.toString, "Нет", "AssignedTo", fromUser)
          case _ => None
        }
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
      if (taskId.toIntOption.getOrElse(0) > 0) {
        ActorManager.issue ! new IssueHistory(taskId.toIntOption.getOrElse(0), userLogin(userId.toIntOption.getOrElse(0)),
        "man_hours", "", hoursAmount, new Date().getTime, "")
      }
      sender() ! res.asJson.noSpaces
    case GetPlanIssues() =>
      sender() ! getIssues.asJson.noSpaces
    case DeletePausedInterval(id) =>
      deletePausedInterval(id)
      sender() ! "success".asJson.noSpaces
    case GetPlanIssue(id) =>
      sender() ! getIssue(id.toIntOption.getOrElse(0)).asJson.noSpaces
    case _ => None
  }
}
