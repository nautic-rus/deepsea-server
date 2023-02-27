package deepsea.time

import akka.actor.Actor
import deepsea.auth.AuthManagerHelper
import deepsea.time.PlanHoursManager.{AssignPlanHoursToUsers, InitPlanHours, PlanHour, SpecialDay}

import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer

object PlanHoursManager{
  case class PlanHour(day: Int, month: Int, year: Int, hour_type: Int, day_type: Int, day_of_week: Int, user: Int = 0, id: Int = 0, task_id: Int = 0)
  case class SpecialDay(day: Int, month: Int, year: Int, kind: String)
  case class InitPlanHours()
  case class AssignPlanHoursToUsers()
}
class PlanHoursManager extends Actor with PlanHoursHelper with AuthManagerHelper {
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
    //self ! InitPlanHours()
    //self ! AssignPlanHoursToUsers()
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

        specialDays.find(x => x.day == day && x.month == month && x.year == year) match {
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

        addPlanHours(hours.toList)


        calendar.add(Calendar.DATE, 1)

      }
      val q = 0
    case AssignPlanHoursToUsers() =>
      val planHours = getTemplatePlanHours
      getUsers.toList.filter(_.visibility.contains("c")).foreach(user => {
        addUserPlanHours(planHours, user.id)
      })
      val q = 0
    case _ => None
  }
}
