package deepsea.time

import deepsea.actors.ActorManager
import deepsea.auth.AuthManagerHelper
import deepsea.database.DBManager
import deepsea.database.DBManager.RsIterator
import deepsea.issues.IssueManager.IssueProject
import deepsea.issues.IssueManagerHelper
import deepsea.mail.MailManager.Mail
import deepsea.time.PlanHoursManager.{AllocatedHour, ConsumedHour, PlanHour, PlannedHours}
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer

trait PlanHoursHelper extends IssueManagerHelper with AuthManagerHelper{
  def fillHours(day: Int, month: Int, year: Int, dayType: Int, dayOfWeek: Int, user: Int): List[PlanHour] = {
    val hour = PlanHour(day, month, year, 1, dayType, dayOfWeek, user)
    dayType match {
      case 1 => List.fill(8)(hour.copy(hour_type = 1)) ++ List.fill(4)(hour.copy(hour_type = 2))
      case 2 => List.fill(7)(hour.copy(hour_type = 1)) ++ List.fill(4)(hour.copy(hour_type = 2))
      case 3 => List.fill(12)(hour.copy(hour_type = 2))
      case _ => List.empty[PlanHour]
    }
  }
  def addTemplatePlanHours(planHours: List[PlanHour]): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        planHours.foreach(p => {
          s.execute(s"insert into hours_template (day, month, year, hour_type, day_type, day_of_week) values (${p.day}, ${p.month}, ${p.year}, ${p.hour_type}, ${p.day_type}, ${p.day_of_week})")
        })
        s.close()
        c.close()
      case _ =>
    }
  }
  def getTemplatePlanHours: List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = "select * from hours_template"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          PlanHour(
            Option(rs.getInt("day")).getOrElse(0),
            Option(rs.getInt("month")).getOrElse(0),
            Option(rs.getInt("year")).getOrElse(0),
            Option(rs.getInt("hour_type")).getOrElse(0),
            Option(rs.getInt("day_type")).getOrElse(0),
            Option(rs.getInt("day_of_week")).getOrElse(0)
          )
        }).toList
        s.close()
        c.close()
        planHours
      case _ => List.empty[PlanHour]
    }
  }
  def addPlanHours(planHours: List[PlanHour], user: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        c.setAutoCommit(false)
        planHours.foreach(p => {
          s.execute(s"insert into hours_template (day, month, year, hour_type, day_type, day_of_week) values (${p.day}, ${p.month}, ${p.year}, ${p.hour_type}, ${p.day_type}, ${p.day_of_week})")
        })
        c.commit()
        s.close()
        c.close()
      case _ =>
    }
  }
  def addUserPlanHours(planHours: List[PlanHour], user: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        c.setAutoCommit(false)
        planHours.foreach(p => {
          s.execute(s"insert into hours_template_user (id, day, month, year, hour_type, day_type, day_of_week, user_id, task_id) values (default, ${p.day}, ${p.month}, ${p.year}, ${p.hour_type}, ${p.day_type}, ${p.day_of_week}, $user, 0)")
        })
        c.commit()
        s.close()
        c.close()
      case _ =>
    }
  }

  def setTaskWithoutMove(userId: Int, taskId: Int, fromHour: Int, amountOfHours: Int): Unit ={
    var hoursAssigned = 0
    var fromHourValue = fromHour
    var hourValue = getHour(fromHourValue, userId)
    while (hoursAssigned < amountOfHours){
      hourValue match {
        case Some(hour) =>
          if (hour.isFree){
            setHour(hour.id, taskId)
            hoursAssigned += 1
          }
          fromHourValue = hour.id
        case _ =>
          fromHourValue += 1
      }
      hourValue = getNextHour(fromHourValue, userId)
    }
  }
  def setTaskWithMove(userId: Int, taskId: Int, fromHourId: Int, amountOfHours: Int): Unit ={
    val userPlanHours = getUserPlanHours(userId, available = true)
    val taskHours = userPlanHours.filter(_.hour_type == 1).filter(_.id >= fromHourId).take(amountOfHours).map(h => AllocatedHour(h.id, h.task_id))
    moveHoursRight(taskHours, userPlanHours)
    DBManager.GetPGConnection() match {
      case Some(c) =>
        c.setAutoCommit(false)
        val s = c.createStatement()
        taskHours.foreach(h => {
          val query = s"update hours_template_user set task_id = $taskId where id = ${h.id}"
          s.execute(query)
        })
        s.close()
        c.commit()
        c.close()
      case _ =>
    }
  }
  def setTaskWithMove(userId: Int, taskId: Int, consumed: List[PlanHour], amountOfHours: Int): Unit ={
    val userPlanHours = getUserPlanHours(userId, available = true)
    val taskHours = userPlanHours.filter(x => consumed.map(_.id).contains(x.id)).map(h => AllocatedHour(h.id, h.task_id))
    moveHoursRight(taskHours, userPlanHours)
    DBManager.GetPGConnection() match {
      case Some(c) =>
        c.setAutoCommit(false)
        val s = c.createStatement()
        taskHours.foreach(h => {
          val query = s"update hours_template_user set task_id = $taskId where id = ${h.id}"
          s.execute(query)
        })
        s.close()
        c.commit()
        c.close()
      case _ =>
    }
  }
  def setUserPlanUpdate(userId: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        val query = s"update users set last_plan_update = $date where id = $userId"
        s.execute(query)
        s.close()
        c.close()
      case _ =>
    }
  }
  def getUserPlanUpdate(userId: Int): Option[Long] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where user_id = $userId"
        val s = c.createStatement()
        val userPlanUpdate = RsIterator(s.executeQuery(query)).map(rs => {
          rs.getLong("plan_last_update")
        }).toList
        s.close()
        c.close()
        userPlanUpdate.headOption
      case _ => Option.empty[Long]
    }
  }
  def checkUserPlanUpdate(userId: Int, planUpdate: Long): Boolean = {
    getUserPlanUpdate(userId) match {
      case Some(planUpdateValue) =>
        setUserPlanUpdate(userId)
        planUpdateValue == planUpdate
      case _ => false
    }
  }
  def moveHoursRight(moving: List[AllocatedHour], userHours: List[PlanHour]): Unit ={
    val allocate = allocateHours(moving, moving.last.id, userHours)
    DBManager.GetPGConnection() match {
      case Some(c) =>
        c.setAutoCommit(false)
        allocate.foreach(h => {
          val query = s"update hours_template_user set task_id = ${h.taskId} where id = ${h.id}"
          c.createStatement().execute(query)
        })
        c.commit()
        c.close()
      case _ =>
    }
  }
  def allocateHours(hoursValue: List[AllocatedHour], fromHourId: Int, userHours: List[PlanHour]): List[AllocatedHour] ={
    if (hoursValue.nonEmpty){
      val hours = hoursValue.sortBy(_.id)
      val nextHours = userHours.filter(_.hour_type == 1).filter(_.id > fromHourId).take(hours.length)
      val assignedHours = nextHours.map(h => AllocatedHour(h.id, hours(nextHours.indexOf(h)).taskId))
      val taskHours = nextHours.filter(_.task_id != 0).map(h => AllocatedHour(h.id, h.task_id))
      assignedHours ++ allocateHours(taskHours, nextHours.last.id, userHours)
    }
    else{
      List.empty[AllocatedHour]
    }
  }
  def getUserPlanHours(userId: Int, startDate: Long = 0, amount: Int = 25, available: Boolean = false): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val calendar = Calendar.getInstance()
        if (startDate != 0){
          calendar.setTime(new Date(startDate))
        }
        else{
          calendar.add(Calendar.DATE, -4)
        }
        val startMonth = calendar.get(Calendar.MONTH)

        val userFilter = s"(user_id = $userId or $userId = 0)"
        val dateFilter = s"((month = $startMonth) or $available)"
        val query = s"select * from hours_template_user where $userFilter and $dateFilter"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          PlanHour(
            Option(rs.getInt("day")).getOrElse(0),
            Option(rs.getInt("month")).getOrElse(0),
            Option(rs.getInt("year")).getOrElse(0),
            Option(rs.getInt("hour_type")).getOrElse(0),
            Option(rs.getInt("day_type")).getOrElse(0),
            Option(rs.getInt("day_of_week")).getOrElse(0),
            Option(rs.getInt("user_id")).getOrElse(0),
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("task_id")).getOrElse(0),
          )
        }).toList
        s.close()
        c.close()
        planHours.sortBy(_.id)
      case _ => List.empty[PlanHour]
    }
  }
  def getNextHoursFromDefined(id: Int, amount: Int, userHours: List[PlanHour], hourType: Int = 1): List[PlanHour] ={
    userHours.filter(_.hour_type == hourType).filter(_.id > id).take(amount)
  }
  def getNextHours(userId: Int, id: Int, amount: Int, hourType: Int = 1): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where user_id = $userId and id > $id and hour_type = $hourType and limit = $amount"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          PlanHour(
            Option(rs.getInt("day")).getOrElse(0),
            Option(rs.getInt("month")).getOrElse(0),
            Option(rs.getInt("year")).getOrElse(0),
            Option(rs.getInt("hour_type")).getOrElse(0),
            Option(rs.getInt("day_type")).getOrElse(0),
            Option(rs.getInt("day_of_week")).getOrElse(0),
            Option(rs.getInt("user_id")).getOrElse(0),
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("task_id")).getOrElse(0),
          )
        }).toList
        s.close()
        c.close()
        planHours.sortBy(_.id)
      case _ => List.empty[PlanHour]
    }
  }
  def getPlannedHours: List[PlannedHours] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where task_id != 0"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          Option(rs.getInt("task_id")).getOrElse(0)
        }).toList
        s.close()
        c.close()
        planHours.groupBy(x => x).map(x => PlannedHours(x._1, x._2.length)).toList
      case _ => List.empty[PlannedHours]
    }
  }
  def getDayHours(userId: Int, day: Int, month: Int, year: Int): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where user_id = $userId and day = $day and month = $month and year = $year"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          PlanHour(
            Option(rs.getInt("day")).getOrElse(0),
            Option(rs.getInt("month")).getOrElse(0),
            Option(rs.getInt("year")).getOrElse(0),
            Option(rs.getInt("hour_type")).getOrElse(0),
            Option(rs.getInt("day_type")).getOrElse(0),
            Option(rs.getInt("day_of_week")).getOrElse(0),
            Option(rs.getInt("user_id")).getOrElse(0),
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("task_id")).getOrElse(0),
          )
        }).toList
        s.close()
        c.close()
        planHours.sortBy(_.id)
      case _ => List.empty[PlanHour]
    }
  }
  def getConsumedHours(userId: Int): List[ConsumedHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_consumed where user_id = $userId or $userId = 0"
        val s = c.createStatement()
        val consumedHours = RsIterator(s.executeQuery(query)).map(rs => {
          ConsumedHour(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("hour_id")).getOrElse(0),
            Option(rs.getInt("user_id")).getOrElse(0),
            Option(rs.getLong("date_inserted")).getOrElse(0),
            Option(rs.getInt("task_id")).getOrElse(0),
            Option(rs.getString("comment")).getOrElse(""),
          )
        }).toList
        s.close()
        c.close()
        consumedHours.sortBy(_.id)
      case _ => List.empty[ConsumedHour]
    }
  }
  def consumeHours(consumed: List[PlanHour], userId: Int, taskId: Int, details: String): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        consumed.foreach(h => {
          val date = new Date().getTime
          val query = s"insert into hours_consumed (id, hour_id, user_id, date_inserted, task_id, comment) values (default, ${h.id}, ${h.user}, $date, ${taskId}, '${details}')"
          s.execute(query)
        })
        s.close()
        c.close()
      case _ =>
    }
    reduceTaskPlannedHours(userId, taskId, consumed.length)
    setTaskWithMove(userId, taskId, consumed.sortBy(_.id), consumed.length)
  }
  def consumePlanHours(jsonValue: String, userId: String, taskId: String, details: String): Unit ={
    decode[List[PlanHour]](jsonValue) match {
      case Right(planHours) =>
        consumeHours(planHours, userId.toIntOption.getOrElse(0), taskId.toIntOption.getOrElse(0), details)
      case _ =>
    }
  }
  def getTaskHours(taskId: Int): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where task_id = $taskId"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          PlanHour(
            Option(rs.getInt("day")).getOrElse(0),
            Option(rs.getInt("month")).getOrElse(0),
            Option(rs.getInt("year")).getOrElse(0),
            Option(rs.getInt("hour_type")).getOrElse(0),
            Option(rs.getInt("day_type")).getOrElse(0),
            Option(rs.getInt("day_of_week")).getOrElse(0),
            Option(rs.getInt("user_id")).getOrElse(0),
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("task_id")).getOrElse(0),
          )
        }).toList
        s.close()
        c.close()
        planHours.sortBy(_.id)
      case _ => List.empty[PlanHour]
    }
  }
  def getNextHour(id: Int, user: Int):  Option[PlanHour] ={
    getHour(id + 1, user)
  }
  def getHour(id: Int, user: Int): Option[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where id = $id and user_id = $user"
        val s = c.createStatement()
        val planHours = RsIterator(s.executeQuery(query)).map(rs => {
          PlanHour(
            Option(rs.getInt("day")).getOrElse(0),
            Option(rs.getInt("month")).getOrElse(0),
            Option(rs.getInt("year")).getOrElse(0),
            Option(rs.getInt("hour_type")).getOrElse(0),
            Option(rs.getInt("day_type")).getOrElse(0),
            Option(rs.getInt("day_of_week")).getOrElse(0),
            Option(rs.getInt("user_id")).getOrElse(0),
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("task_id")).getOrElse(0),
          )
        }).toList
        s.close()
        c.close()
        planHours.headOption
      case _ => Option.empty[PlanHour]
    }
  }
  def setHour(id: Int, taskId: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"update hours_template_user set task_id = $taskId where id = $id"
        c.createStatement().execute(query)
        c.close()
      case _ =>
    }
  }
  def deleteUserTask(userId: Int, taskId: Int, fromHour: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"update hours_template_user set task_id = 0 where task_id = $taskId and (id >= $fromHour or $fromHour = 0) and user_id = $userId"
        c.createStatement().execute(query)
        c.close()
      case _ =>
    }
  }
  def reduceTaskPlannedHours(userId: Int, taskId: Int, amount: Int): Unit ={
    val plannedHours = getUserPlanHours(userId, available = true)
    val hoursToRemove = plannedHours.filter(_.task_id == taskId).takeRight(amount)
    if (hoursToRemove.nonEmpty){
      val idsToRemove = hoursToRemove.map(_.id).mkString(",")
      DBManager.GetPGConnection() match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"update hours_template_user set task_id = 0 where id in ($idsToRemove)"
          s.execute(query)
          s.close()
          c.close()
        case _ =>
      }
      val firstHour = hoursToRemove.head
      foldTasksLeft(userId, firstHour.id)
    }
  }
  def foldTasksLeft(userId: Int, fromHourId: Int): Unit ={
    val plannedHours = getUserPlanHours(userId, available = true)
    val plannedTasks = plannedHours.filter(_.id >= fromHourId).filter(_.task_id != 0)
    val plannedTasksIds = plannedTasks.map(_.id).mkString(",")
    if (plannedTasksIds.nonEmpty){
      DBManager.GetPGConnection() match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"update hours_template_user set task_id = 0 where id in ($plannedTasksIds)"
          s.execute(query)
          s.close()
          c.close()
        case _ =>
      }
      plannedTasks.sortBy(_.id).groupBy(_.task_id).foreach(gr => {
        setTaskWithoutMove(userId, gr._1, fromHourId, gr._2.length)
      })
    }
  }

  def consumeTodayHours(): Unit = {
    val today = Calendar.getInstance()
    val day = today.get(Calendar.DAY_OF_MONTH)
    val month = today.get(Calendar.MONTH)
    val year = today.get(Calendar.YEAR)
    ActorManager.mail ! Mail("Bogdan Isaev", "redeeming.fury@gmail.com", "Hours Consumption Notification", s"Hours Consumption for $day/$month/$year completed.")

    val planHours = getUserPlanHours(0, available = true)
    val consumed = getConsumedHours(0)
    val todayPlanHours = planHours.filter(x => x.day == day && x.month == month && x.year == year).filter(_.task_id != 0)
    val notConsumedTodayPlanHours = todayPlanHours.filter(x => !consumed.map(_.hour_id).contains(x.id))
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        notConsumedTodayPlanHours.foreach(h => {
          val date = new Date().getTime
          val query = s"insert into hours_consumed (id, hour_id, user_id, date_inserted, task_id, comment) values (default, ${h.id}, ${h.user}, $date, ${h.task_id}, 'DAILY AUTO HOURS CONSUMPTION')"
          s.execute(query)
        })
        s.close()
        c.close()
      case _ =>
    }
    todayPlanHours.groupBy(x => (x.task_id, x.user)).foreach(gr => {
      getIssueDetails(gr._1._1) match {
        case Some(issue) =>
          setIssueLabor(issue.id, new Date().getTime, gr._2.length, "DAILY AUTO HOURS CONSUMPTION", issue.assigned_to, new Date().getTime, issue.id, issue.project)
        case _ => None
      }
    })
  }
}
