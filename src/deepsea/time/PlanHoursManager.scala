package deepsea.time

import akka.actor.Actor
import deepsea.auth.AuthManagerHelper
import deepsea.database.MongoCodecs
import deepsea.issues.IssueManagerHelper
import deepsea.issues.classes.Issue
import deepsea.time.PlanHoursManager.{AssignPlanHoursToUsers, DeleteUserTask, GetUserPlanHours, InitPlanHours, PlanAlreadyPlannedIssues, PlanHour, PlanUserTask, SpecialDay}
import io.circe.syntax.EncoderOps

import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer

object PlanHoursManager{
  case class PlanHour(day: Int, month: Int, year: Int, hour_type: Int, day_type: Int, day_of_week: Int, user: Int = 0, id: Int = 0, task_id: Int = 0){
    def isFree: Boolean = {
      hour_type == 1 && task_id == 0
    }
  }
  case class AllocatedHour(id: Int, taskId: Int)
  case class SpecialDay(day: Int, month: Int, year: Int, kind: String)
  case class InitPlanHours()
  case class AssignPlanHoursToUsers()
  case class GetUserPlanHours(userId: String, startDate: String, available: String)
  case class PlanUserTask(userId: String, taskId: String, fromHour: String, amountOfHours: String, allowMove: String)
  case class DeleteUserTask(userId: String, taskId: String, fromHour: String)
  case class PlanAlreadyPlannedIssues()
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
  )
  override def preStart(): Unit = {
    //getUserPlanHours(0)
    //self ! InitPlanHours()
    //self ! AssignPlanHoursToUsers()
    //self ! PlanAlreadyPlannedIssues()
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
    case AssignPlanHoursToUsers() =>
      val planHours = getTemplatePlanHours
      getUsers.toList.filter(_.visibility.contains("c")).foreach(user => {
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
    case DeleteUserTask(userId, taskId, fromHour) =>
      deleteUserTask(userId.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0), fromHour.toIntOption.getOrElse(0))
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
    case _ => None
  }
}
