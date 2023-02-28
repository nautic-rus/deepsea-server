package deepsea.time

import deepsea.database.DBManager
import deepsea.database.DBManager.RsIterator
import deepsea.time.PlanHoursManager.{AllocatedHour, PlanHour}

import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer

trait PlanHoursHelper {
  def fillHours(day: Int, month: Int, year: Int, dayType: Int, dayOfWeek: Int, user: Int): List[PlanHour] = {
    val hour = PlanHour(day, month, year, 1, dayType, dayOfWeek, user)
    dayType match {
      case 1 => List.fill(8)(hour.copy(hour_type = 1)) ++ List.fill(4)(hour.copy(hour_type = 2))
      case 2 => List.fill(7)(hour.copy(hour_type = 1)) ++ List.fill(4)(hour.copy(hour_type = 2))
      case 3 => List.fill(12)(hour.copy(hour_type = 2))
      case _ => List.empty[PlanHour]
    }
  }
  def addPlanHours(planHours: List[PlanHour]): Unit ={
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
  def setTask(hourId: Int, userId: Int, taskId: Int, amountOfHours: Int): Unit ={
    var hoursAssigned = amountOfHours
    var hourValue = getHour(hourId, userId)
    while (hoursAssigned > 0 || hourValue.nonEmpty){
      hourValue match {
        case Some(hour) =>
          if (hour.isFree){
            setHour(hour.id, taskId)
            hoursAssigned += 1
          }
        case _ =>
      }
      hourValue = getNextHour(hourId, userId)
    }
  }
  def setTaskWithMove(hourId: Int, userId: Int, taskId: Int, amountOfHours: Int): Unit ={
    var hoursAssigned = amountOfHours
    var hourValue = getHour(hourId, userId)
    while (hoursAssigned > 0 || hourValue.nonEmpty){
      hourValue match {
        case Some(hour) =>
          if (hour.isFree){
            setHour(hour.id, taskId)
            hoursAssigned += 1
          }
        case _ =>
      }
      hourValue = getNextHour(hourId, userId)
    }
  }
  def moveHour(hourId: Int, userId: Int): Unit ={
    getNextHour(hourId, userId) match {
      case Some(nextHour) =>
        if (nextHour.isFree){

        }
      case _ =>
    }
  }


  def setUserPlanUpdate(userId: Int): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val date = new Date().getTime
        val query = s"update users set last_plan_update = $date where id = $userId"
        c.createStatement().execute(query)
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
  def moveTaskRight(taskId: Int, amount: Int, userId: Int): Unit ={
    val userHours = getUserPlanHours(userId)
    val taskHours = userHours.filter(_.id == taskId)
    val moving = taskHours.take(amount).map(h => AllocatedHour(h.id, h.task_id))
    val allocate = allocateHours(moving, taskHours.last.id, userHours)
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
    val hours = hoursValue.sortBy(_.id)
    val nextHours = userHours.filter(_.hour_type == 1).filter(_.id > fromHourId).take(hours.length)
    val assignedHours = nextHours.map(h => AllocatedHour(h.id, hours(nextHours.indexOf(h)).taskId))
    val taskHours = nextHours.filter(_.task_id != 0).map(h => AllocatedHour(h.id, h.task_id))
    assignedHours ++ allocateHours(taskHours, nextHours.last.id, userHours)
  }
  def getUserPlanHours(userId: Int, startDate: Long = 0, amount: Int = 31): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val calendar = Calendar.getInstance()
        if (startDate != 0){
          calendar.setTime(new Date(startDate))
        }
        else{
          calendar.add(Calendar.DATE, -7)
        }
        val startDay = calendar.get(Calendar.DATE)
        val startMonth = calendar.get(Calendar.MONTH)
        val startYear = calendar.get(Calendar.YEAR)

        calendar.add(Calendar.DATE, amount)
        val endDay = calendar.get(Calendar.DATE)
        val endMonth = calendar.get(Calendar.MONTH)
        val endYear = calendar.get(Calendar.YEAR)

        val userFilter = s"(user_id = $userId or $userId = 0)"
        val dateFilter = s"(day >= $startDay and day <= $endDay and month >= $startMonth and month <= $endMonth and year <= $startYear and year >= $endYear)"
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
  def getNextHours(id: Int, amount: Int, hourType: Int = 1): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where id > $id and hour_type = $hourType and limit = $amount"
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
  def getTaskHours(taskId: Int): List[PlanHour] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select * from hours_template_user where taskId = $taskId"
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
}
