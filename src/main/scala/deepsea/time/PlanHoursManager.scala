package deepsea.time

import akka.actor.{Actor, ActorSystem}
import deepsea.App
import deepsea.auth.AuthManager.User
import deepsea.auth.AuthManagerHelper
import deepsea.dbase.MongoCodecs
import deepsea.issues.IssueManagerHelper
import deepsea.issues.classes.Issue
import deepsea.time.PlanHoursManager.{AssignPlanHoursToUsers, ConsumePlanHours, ConsumeTodayPlanHours, DeleteUserTask, FillConsumed, GetConsumedHours, GetPlannedHours, GetUserPlanHours, InitPlanHours, PlanAlreadyPlannedIssues, PlanHour, PlanUserTask, SpecialDay, SavePlannedHours}
import io.circe.syntax.EncoderOps

import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt

object PlanHoursManager extends MongoCodecs {
  case class PlanHour(day: Int, month: Int, year: Int, hour_type: Int, day_type: Int, day_of_week: Int, user: Int = 0, id: Int = 0, task_id: Int = 0){
    def isFree: Boolean = {
      hour_type == 1 && task_id == 0
    }
  }
  case class AllocatedHour(id: Int, taskId: Int)
  case class SpecialDay(day: Int, month: Int, year: Int, kind: String)
  case class InitPlanHours()
  case class AssignPlanHoursToUsers(id: Int)
  case class GetUserPlanHours(userId: String, startDate: String, available: String)
  case class PlanUserTask(userId: String, taskId: String, fromHour: String, amountOfHours: String, allowMove: String)
  case class DeleteUserTask(userId: String, taskId: String, fromHour: String, fromUser: String)
  case class ConsumedHour(id: Int, hour_id: Int, user_id: Int, date_inserted: Long, task_id: Int, comment: String)
  case class PlanAlreadyPlannedIssues()
  case class FillConsumed()
  case class GetPlannedHours()
  case class GetConsumedHours(userId: String)
  case class SavePlannedHours(userId: String, taskId: String, value: String, plan: String)
  case class PlannedHours(taskId: Int, hours: Int)
  case class ConsumePlanHours(planHoursValue: String, userId: String, taskId: String, details: String)
  case class ConsumeTodayPlanHours()
}
class PlanHoursManager extends Actor with PlanHoursHelper with AuthManagerHelper with IssueManagerHelper with MongoCodecs{
  val specialDays: List[SpecialDay] = List(
    SpecialDay(2, 1, 2023, "weekend"),
    SpecialDay(3, 1, 2023, "weekend"),
    SpecialDay(4, 1, 2023, "weekend"),
    SpecialDay(5, 1, 2023, "weekend"),
    SpecialDay(6, 1, 2023, "weekend"),
    SpecialDay(22, 2, 2023, "short"),
    SpecialDay(23, 2, 2023, "weekend"),
    SpecialDay(7, 3, 2023, "short"),
    SpecialDay(8, 3, 2023, "weekend"),
    SpecialDay(1, 5, 2023, "weekend"),
    SpecialDay(8, 5, 2023, "weekend"),
    SpecialDay(9, 5, 2023, "weekend"),
    SpecialDay(12, 6, 2023, "weekend"),
    SpecialDay(3, 11, 2023, "short"),
    SpecialDay(6, 11, 2023, "weekend"),

    SpecialDay(1, 1, 2024, "weekend"),
    SpecialDay(2, 1, 2024, "weekend"),
    SpecialDay(3, 1, 2024, "weekend"),
    SpecialDay(4, 1, 2024, "weekend"),
    SpecialDay(5, 1, 2024, "weekend"),
    SpecialDay(8, 1, 2024, "weekend"),
    SpecialDay(22, 2, 2024, "short"),
    SpecialDay(23, 2, 2024, "weekend"),
    SpecialDay(7, 3, 2024, "short"),
    SpecialDay(8, 3, 2024, "weekend"),
    SpecialDay(27, 4, 2024, "working"),
    SpecialDay(29, 4, 2024, "weekend"),
    SpecialDay(30, 4, 2024, "weekend"),
    SpecialDay(1, 5, 2024, "weekend"),
    SpecialDay(8, 5, 2024, "short"),
    SpecialDay(9, 5, 2024, "weekend"),
    SpecialDay(10, 5, 2024, "weekend"),
    SpecialDay(11, 6, 2024, "short"),
    SpecialDay(12, 6, 2024, "weekend"),
    SpecialDay(2, 11, 2024, "short"),
    SpecialDay(4, 11, 2024, "weekend"),
    SpecialDay(28, 12, 2024, "working"),
    SpecialDay(30, 12, 2024, "weekend"),
    SpecialDay(31, 12, 2024, "weekend"),

  )
  implicit val system: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val executor: ExecutionContextExecutor = system.dispatcher
  override def preStart(): Unit = {
    //system.scheduler.scheduleWithFixedDelay(0.seconds, 60.minutes, self, ConsumeTodayPlanHours())
    //getUserPlanHours(0)
    //self ! InitPlanHours()
//    self ! AssignPlanHoursToUsers(267)
//    self ! AssignPlanHoursToUsers(268)
//    val q = 0
    //self ! PlanAlreadyPlannedIssues()
    //self ! FillConsumed()
  }

  override def receive: Receive = {
    case InitPlanHours() =>
      val calendar = Calendar.getInstance()
      calendar.set(2023, 0, 1)
      while (calendar.get(Calendar.YEAR) != 2024){
        val dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK)
        val day = calendar.get(Calendar.DATE)
        val month = calendar.get(Calendar.MONTH)
        val year = calendar.get(Calendar.YEAR)

        val hours = ListBuffer.empty[PlanHour]
        val user = 0

        specialDays.find(x => x.day == day && x.month - 1 == month && x.year == year) match {
          case Some(specialDay: SpecialDay) =>
            specialDay.kind match {
              case "ordinary" =>
                hours ++= fillHours(day, month, year, 1, dayOfWeek, user)
              case "short" =>
                hours ++= fillHours(day, month, year, 2, dayOfWeek, user)
              case "weekend" =>
                hours ++= fillHours(day, month, year, 3, dayOfWeek, user)
            }
          case _ =>
            if (List(1, 7).contains(dayOfWeek)){
              hours ++= fillHours(day, month, year, 3, dayOfWeek, user)
            }
            else{
              hours ++= fillHours(day, month, year, 1, dayOfWeek, user)
            }
        }

        addPlanHours(hours.toList, user)


        calendar.add(Calendar.DATE, 1)

      }
      val q = 0
    case AssignPlanHoursToUsers(id) =>
      val planHours = getTemplatePlanHours
      getUsers.filter(_.id == id).foreach(user => {
        addUserPlanHours(planHours, user.id)
      })
      val q = 0
    case GetUserPlanHours(userId, startDate, available) =>
      sender() ! getUserPlanHours(userId.toIntOption.getOrElse(0), startDate = startDate.toLongOption.getOrElse(0), available = available.toIntOption.getOrElse(0) == 1).asJson.noSpaces
    case PlanUserTask(userId, taskId, fromHour, amountOfHours, allowMove) =>
      if (allowMove.toIntOption.getOrElse(0) == 1){
        setTaskWithMove(userId.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0), fromHour.toIntOption.getOrElse(0), amountOfHours.toIntOption.getOrElse(0))
      }
      else{
        setTaskWithoutMove(userId.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0), fromHour.toIntOption.getOrElse(0), amountOfHours.toIntOption.getOrElse(0))
      }
      sender() ! "success".asJson.noSpaces
    case DeleteUserTask(userId, taskId, fromHour, fromUser) =>
      deleteUserTask(userId.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0), fromHour.toIntOption.getOrElse(0), fromUser)
      sender() ! "success".asJson.noSpaces
    case PlanAlreadyPlannedIssues() =>
      val users = getUsers
      val issues: List[Issue] = users.find(_.login == "op") match {
        case Some(user) => getIssuesForUser(user).toList
        case _ => List.empty[Issue]
      }
      val assignIssues = issues.filter(_.plan_hours != 0).filter(_.assigned_to != "").filter(_.start_date != 0)
      val qwe = 0
      assignIssues.foreach(issue => {
        users.find(_.login == issue.assigned_to) match {
          case Some(assignee) =>
            val date = issue.start_date
            val calendar = Calendar.getInstance()
            calendar.setTime(new Date(date))
            val day = calendar.get(Calendar.DATE)
            val month = calendar.get(Calendar.MONTH)
            val year = calendar.get(Calendar.YEAR)
            val dayHours = getDayHours(assignee.id, day, month, year).filter(_.hour_type == 1).sortBy(_.id)
            val task = dayHours.find(!_.isFree)
            if (task.nonEmpty){
              println("Task #" + issue.id + " with doc_number " + issue.doc_number + " moved because day " + day + "/" + (month + 1) + "/" + year + " already contains task with #" + task.get.id)
            }
            println(assignIssues.indexOf(issue) + " from " + assignIssues.length)
            if (dayHours.nonEmpty){
              setTaskWithoutMove(assignee.id, issue.id, dayHours.head.id, Math.round(issue.plan_hours).toInt)
              val taskHours = getTaskHours(issue.id)
              val latest = taskHours.sortBy(_.id).last
              val calendarEndDate = Calendar.getInstance()
              calendarEndDate.set(Calendar.YEAR, latest.year)
              calendarEndDate.set(Calendar.MONTH, latest.month)
              calendarEndDate.set(Calendar.DATE, latest.day)
              if (issue.contract_due_date != 0 && calendarEndDate.getTime.getTime > issue.contract_due_date){
                println("Task #" + issue.id + " with doc_number " + issue.doc_number + " goes out of plan. Plan: " + new Date(issue.contract_due_date).toLocaleString + ", actual: " + calendarEndDate.getTime.toLocaleString)
              }
            }
          case _ => None
        }
      })
      val qa = 0
    case FillConsumed() =>
      val userFull = ListBuffer.empty[Tuple4[Int, Int, Int, Int]]
      val users = ListBuffer.empty[User]
      val st = getIssueSpentTime
      st.foreach(t => {
        println(st.indexOf(t) + "/" + st.length)
        val c = Calendar.getInstance()
        c.setTime(new Date(t.date))
        val d = c.get(Calendar.DATE)
        val m = c.get(Calendar.MONTH)
        val y = c.get(Calendar.YEAR)
        if (y == 2023){
          (users.find(_.login == t.userLogin) match {
            case Some(value) => Option(value)
            case _ => getUser(t.userLogin)
          }) match {
            case Some(u) =>
              users += u
              if (userFull.find(x => x._1 == d && x._2 == m && x._3 == y && x._4 == u.id) != null){
                val dayHours = getDayHours(u.id, d, m, y).filter(_.hour_type == 1)
                val consumed = getConsumedHours(u.id).map(_.hour_id)
                val free = dayHours.filterNot(x => consumed.contains(x.id))
                val amount = Math.floor(t.time).toInt
                val taskId = if (t.issueId != 0){
                  t.issueId
                }
                else{
                  -10
                }
                if (free.nonEmpty){
                  consumeHours(free.take(if (amount > free.length) free.length else amount), u.id, t.issueId, t.details)
                }
                if (free.isEmpty){
                  userFull += Tuple4(d, m, y, u.id)
                }
              }
            case _ =>
          }
        }
      })
      val q = 0
    case GetPlannedHours() =>
      sender() ! getPlannedHours.asJson.noSpaces
    case GetConsumedHours(userId) =>
      sender() ! getConsumedHours(userId.toIntOption.getOrElse(0)).asJson.noSpaces
    case ConsumePlanHours(planHoursValue, userId, taskId, details) =>
      consumePlanHours(planHoursValue, userId, taskId, details)
      sender() ! "success".asJson.noSpaces
    case SavePlannedHours(userId, taskId, value, plan) =>
      savePlannedHours(userId.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0), value.toIntOption.getOrElse(0), plan.toIntOption.getOrElse(0))
      sender() ! "success".asJson.noSpaces

    case ConsumeTodayPlanHours() =>
      val c = Calendar.getInstance()
      val hour = c.get(Calendar.HOUR_OF_DAY)
      val minute = c.get(Calendar.MINUTE)
      if (hour == 23 && minute == 0 && App.HTTPServer.Host == "192.168.1.28"){
        //consumeTodayHours()
      }
    case _ => None
  }
}
