package deepsea.time

import akka.actor.Actor
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager
import deepsea.database.{DBManager, MongoCodecs}
import deepsea.issues.IssueManager.AssignIssue
import deepsea.issues.classes.IssueHistory
import deepsea.materials.MaterialManager.MaterialHistory
import deepsea.time.PlanManager._
import io.circe
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps

import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer

object PlanManager{
  case class GetPlan()
  case class GetPlanByDays(date: String)
  case class GetUserPlan(user: String, from: String)
  case class GetPlanNotOrdinary(from: String)
  case class PlanInterval(id: Int, task_id: Int, user_id: Int, date_start: Long, date_finish: Long, task_type: Int, hours_amount: Int, consumed: Int)
  case class AddInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String, fromUser: String)
  case class InsertInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String, fromUser: String)
  case class InsertConsumedInterval(taskId: String, userId: String, from: String, hoursAmount: String, taskType: String)
  case class DayInterval(taskId: Int, hours: Int, hours_total: Int, id: Int, date_start: Long, consumed: Int, taskType: Int)
  case class PlanByDays(day: Int, month: Int, year: Int, ints: List[DayInterval])
  case class UserPlan(userId: Int, plan: List[PlanByDays])
  case class DeleteInterval(id: String, fromUser: String)
  case class IssuePlan(id: Int, name: String, docNumber: String,
                       plan: Int, status: String, issue_type: String, period: String,
                       assigned_to: String, project: String, department: String,
                       closing_status: String, stage_date: Long,
                       consumed: Double, inPlan: Int, available: Int, available_limit: Int)

  implicit val IssuePlanDecoder: Decoder[IssuePlan] = deriveDecoder[IssuePlan]
  implicit val IssuePlanEncoder: Encoder[IssuePlan] = deriveEncoder[IssuePlan]

  case class GetPlanIssues()
  case class GetPlanIssue(id: String)
  case class DeletePausedInterval(id: Int)
  case class UserTCID(id: Int, tcid: Int)
  case class GetUserStats(dateFrom: String, dateTo: String, users: String)
  case class GetProjectStats(project: String, docType: String)
  case class GetUserDiary(userId: String)
  case class DeleteFromDiary(id: String)
  case class GetConsumedPlan(userId: String)


  case class UserStats(id: Int, tcId: Int, plan: Int, office: Int, tasks: Double, vacation: Int, medical: Int, dayOff: Int, study: Int, details: List[UserStatsDetails])
  implicit val UserStatsDecoder: Decoder[UserStats] = deriveDecoder[UserStats]
  implicit val UserStatsEncoder: Encoder[UserStats] = deriveEncoder[UserStats]

  case class UserStatsDetails(dateLong: Long, dateString: String, officeTime: Double, officeTimeStr: String, tasks: List[UserStatsDetailsTask], special: String)
  implicit val UserStatsDetailsDecoder: Decoder[UserStatsDetails] = deriveDecoder[UserStatsDetails]
  implicit val UserStatsDetailsEncoder: Encoder[UserStatsDetails] = deriveEncoder[UserStatsDetails]

  case class UserStatsDetailsTask(id: Int, issueType: String, name: String, docNumber: String, hours: Double)
  implicit val UserStatsDetailsTaskDecoder: Decoder[UserStatsDetailsTask] = deriveDecoder[UserStatsDetailsTask]
  implicit val UserStatsDetailsTaskEncoder: Encoder[UserStatsDetailsTask] = deriveEncoder[UserStatsDetailsTask]

  case class ConsumedHours(id: Int, task_id: Int, user_id: Int, amount: Double, date_consumed: Long, date_created: Long)
  implicit val ConsumedHoursDecoder: Decoder[ConsumedHours] = deriveDecoder[ConsumedHours]
  implicit val ConsumedHoursEncoder: Encoder[ConsumedHours] = deriveEncoder[ConsumedHours]

  case class UserDiary(interval: ConsumedHours, issue: IssuePlan)
  implicit val UserDiaryDecoder: Decoder[UserDiary] = deriveDecoder[UserDiary]
  implicit val UserDiaryEncoder: Encoder[UserDiary] = deriveEncoder[UserDiary]

  case class UserNotOrdinaryInterval(userId: Int, plan: Double)
  implicit val UserNotOrdinaryIntervalDecoder: Decoder[UserNotOrdinaryInterval] = deriveDecoder[UserNotOrdinaryInterval]
  implicit val UserNotOrdinaryIntervalEncoder: Encoder[UserNotOrdinaryInterval] = deriveEncoder[UserNotOrdinaryInterval]


  case class DMY(day: Int, month: Int, year: Int)

  case class ProjectStats(project: String, typeDoc: String, departments: List[String], statuses: List[String], periods: List[String], manHoursProgress: List[ManHoursProgress], documentProgress: List[DocumentsProgress], stageProgress: List[StageProgress])
  implicit val ProjectStatsDecoder: Decoder[ProjectStats] = deriveDecoder[ProjectStats]
  implicit val ProjectStatsEncoder: Encoder[ProjectStats] = deriveEncoder[ProjectStats]

  case class ManHoursProgress(department: String, plan: Int, actual: Double, percentage: Double)
  implicit val ManHoursProgressDecoder: Decoder[ManHoursProgress] = deriveDecoder[ManHoursProgress]
  implicit val ManHoursProgressEncoder: Encoder[ManHoursProgress] = deriveEncoder[ManHoursProgress]

  case class DocumentsProgress(department: String, docProgressStatus: List[DocumentProgressStatus], percentage: Int)
  implicit val DocumentsProgressDecoder: Decoder[DocumentsProgress] = deriveDecoder[DocumentsProgress]
  implicit val DocumentsProgressEncoder: Encoder[DocumentsProgress] = deriveEncoder[DocumentsProgress]

  case class DocumentProgressStatus(status: String, value: Int)
  implicit val DocumentProgressStatusDecoder: Decoder[DocumentProgressStatus] = deriveDecoder[DocumentProgressStatus]
  implicit val DocumentProgressStatusEncoder: Encoder[DocumentProgressStatus] = deriveEncoder[DocumentProgressStatus]

  case class StageProgress(department: String, stages: List[StageProgressValue])
  implicit val StageProgressDecoder: Decoder[StageProgress] = deriveDecoder[StageProgress]
  implicit val StageProgressEncoder: Encoder[StageProgress] = deriveEncoder[StageProgress]

  case class StageProgressValue(stage: String, all: Int, delivered: Int)
  implicit val StageProgressValueDecoder: Decoder[StageProgressValue] = deriveDecoder[StageProgressValue]
  implicit val StageProgressValueEncoder: Encoder[StageProgressValue] = deriveEncoder[StageProgressValue]

  case class AddManHours(taskId: String, userId: String, dateConsumed: String, hoursAmount: String, comment: String)
}
class PlanManager extends Actor with PlanManagerHelper with MongoCodecs {
  override def preStart(): Unit = {
    //getPlanNotOrdinary(1696148526000L)
    //getUserStats(1696148526000L, 1698740526000L, List(264))
    //fillNewManHours()
    self ! GetProjectStats("NR002", "RKD")
  }
  def fillNewManHours(): Unit = {
    getPlan.filter(_.consumed == 1).filter(_.date_start != 0).filter(_.task_type == 0).filter(_.hours_amount > 0).foreach(c => {
      addManHours(c.task_id, c.user_id, c.date_start, c.hours_amount, "INITIAL", check = false)
    })
    val q = 0
  }
  def fillPrevCalendar(): Unit = {
    val caltoday = Calendar.getInstance()
    caltoday.set(caltoday.get(Calendar.YEAR), caltoday.get(Calendar.MONTH), caltoday.get(Calendar.DAY_OF_MONTH), 8, 0, 0)
    val today = caltoday.getTime.getTime
    getUsers.foreach(u => {
      val plan = getUserPlanHours(u, 0, 0, true)
      val planBefore = plan.filter(_.task_id > 0).filter(hour => {
        val cal = Calendar.getInstance()
        cal.set(hour.year, hour.month, hour.day, 8, 0, 0)
        val date = cal.getTime.getTime
        date < today
      })
      val planFurther = plan.filter(_.task_id != 0).filter(hour => {
        val cal = Calendar.getInstance()
        cal.set(hour.year, hour.month, hour.day, 8, 0, 0)
        val date = cal.getTime.getTime
        date >= today
      })

      planBefore.groupBy(_.task_id).toList.foreach(gr => {
        DBManager.GetPGConnection() match {
          case Some(c) =>
            val s = c.createStatement()
            val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount, consumed) values (${gr._1}, $u, 0, 0, 0, ${gr._2.length}, 1)"
            s.execute(query)
            s.close()
            c.close()
          case _ => None
        }
      })
      planFurther.groupBy(_.task_id).toList.sortBy(_._2.head.id).foreach(gr => {
        val taskType = gr._1 match {
          case -2 => 2
          case -1 => 1
          case _ => 0
        }
        addInterval(gr._1, u, today, gr._2.length, taskType)
      })
      val q = 0
    })
    val jk = 0

    //      var prevTask = 0
    //      val hours = ListBuffer.empty[Long]
    //      var startHour: Long = 0
    //      plan.sortBy(_.id).foreach(hour => {
    //        if (prevTask == 0){
    //          prevTask = hour.task_id
    //          val cal = Calendar.getInstance()
    //          cal.set(hour.year, hour.month, hour.day, 8, 0, 0)
    //          val todayStart = cal.getTime.getTime
    //          startHour = nextHour(todayStart)
    //        }
    //        if (hours.nonEmpty && hours.last < today && startHour > today){
    //          addIntervalManual(prevTask, u, hours.head, hours.last, hours.length, 0, 1)
    //          hours.clear()
    //          hours += startHour
    //          startHour = nextHour(startHour)
    //          prevTask = hour.task_id
    //        }
    //        else if (prevTask == hour.task_id){
    //          hours += startHour
    //          startHour = nextHour(startHour)
    //        }
    //        else{
    //          val consumed = if (hours.last < today) 1 else 0
    //          addIntervalManual(prevTask, u, hours.head, hours.last, hours.length, 0, consumed)
    //          hours.clear()
    //          hours += startHour
    //          startHour = nextHour(startHour)
    //          prevTask = hour.task_id
    //        }
    //        if (hours.nonEmpty && plan.last == hour) {
    //          val consumed = if (hours.last < today) 1 else 0
    //          addIntervalManual(hour.task_id, hour.user, hours.head, hours.last, hours.length, 0, consumed)
    //          hours.clear()
    //        }
    //      })
  }
  override def receive: Receive = {
    case GetPlan() =>
      sender() ! getPlan.asJson.noSpaces
    case GetPlanByDays(date) =>
      sender() ! getPlanByDays(date.toLongOption.getOrElse(0)).asJson.noSpaces
    case GetUserPlan(user, from) =>
      sender() ! getUserPlan(user.toIntOption.getOrElse(0), from.toLongOption.getOrElse(0)).asJson
    case GetPlanNotOrdinary(from: String) => (from)
      sender() ! getPlanNotOrdinary(from.toLongOption.getOrElse(0)).asJson.noSpaces
    case DeleteInterval(id, fromUser) =>
      getInterval(id.toIntOption.getOrElse(0)).headOption match {
        case Some(int) =>
          if (int.task_id > 0) {
            ActorManager.issue ! AssignIssue(int.task_id.toString, "", 0.toString, 0.toString, "Нет", "New", fromUser, "1")
          }
        case _ => None
      }
      deletePausedIntervalByIntervalId(id.toIntOption.getOrElse(0))
      //deleteInterval(id.toIntOption.getOrElse(0))
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
            val intervals = getTaskPlan(int.task_id).filter(_.date_start != 0)
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
            val intervals = getTaskPlan(int.task_id).filter(_.date_start != 0)
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
        case -3 => "error: критическая ошибка"
        case -2 => "error: невозможно добавить задачу на день в котором есть списанные интервалы"
        case -1 => "error: неверная дата"
        case _ => "success"
      }
      sender() ! response.asJson.noSpaces
    case InsertConsumedInterval(taskId, userId, from, hoursAmount, taskType) =>
      val res = insertConsumedInterval(taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        from.toLongOption.getOrElse(0),
        hoursAmount.toIntOption.getOrElse(0),
        taskType.toIntOption.getOrElse(0))
      if (taskId.toIntOption.getOrElse(0) > 0 && res == "success") {
        ActorManager.issue ! new IssueHistory(taskId.toIntOption.getOrElse(0), userLogin(userId.toIntOption.getOrElse(0)),
        "man_hours", "", hoursAmount, new Date().getTime, "")
      }
      sender() ! res.asJson.noSpaces
    case AddManHours(taskId, userId, dateConsumed, hoursAmount, comment) =>
      val res = addManHours(
        taskId.toIntOption.getOrElse(0),
        userId.toIntOption.getOrElse(0),
        dateConsumed.toLongOption.getOrElse(0),
        hoursAmount.toDoubleOption.getOrElse(0),
        comment
      )
      if (taskId.toIntOption.getOrElse(0) > 0 && res == "success") {
        ActorManager.issue ! new IssueHistory(taskId.toIntOption.getOrElse(0), userLogin(userId.toIntOption.getOrElse(0)),
          "man_hours", "", hoursAmount, new Date().getTime, comment)
      }
      sender() ! res.asJson.noSpaces
    case GetPlanIssues() =>
      sender() ! getIssues.asJson.noSpaces
    case DeletePausedInterval(id) =>
      deletePausedIntervalByTaskId(id)
      sender() ! "success".asJson.noSpaces
    case GetPlanIssue(id) =>
      sender() ! getIssue(id.toIntOption.getOrElse(0)).asJson.noSpaces
    case GetUserStats(dateFrom, dateTo, users) =>
      circe.jawn.decode[List[Int]](users) match {
        case Right(usersIds) =>
          sender() ! getUserStats(dateFrom.toLongOption.getOrElse(0), dateTo.toLongOption.getOrElse(0), usersIds).asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case GetProjectStats(project, docType) =>
      sender() ! getProjectStats(project, docType).asJson.noSpaces
    case GetUserDiary(userId: String) =>
      sender() ! getUserDiary(userId.toIntOption.getOrElse(0)).asJson.noSpaces
    case DeleteFromDiary(id: String) =>
      deleteFromDiary(id.toIntOption.getOrElse(0))
      sender() ! "success".asJson.noSpaces
    case GetConsumedPlan(userId: String) =>
      sender() ! getConsumedHours(userId.toIntOption.getOrElse(0)).asJson.noSpaces
    case _ => None
  }
}
