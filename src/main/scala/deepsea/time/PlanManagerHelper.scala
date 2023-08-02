package deepsea.time

import deepsea.database.DBManager
import deepsea.database.DBManager.RsIterator
import deepsea.time.PlanHoursManager.SpecialDay
import deepsea.time.PlanManager.PlanInterval

import java.util.Date
import scala.collection.mutable.ListBuffer

trait PlanManagerHelper {

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
  val msOneHour = 3600 * 1000
  def getPlan: List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
          )
        }).toList
        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }


  def addInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int): Unit = {
    val dateFrom = nextHourLatest(userId, from)
    val hours = ListBuffer.empty[Long]
    var h = dateFrom
    hours += h
    while (hours.length < hoursAmount){
      h = nextHour(h)
      hours += h
    }
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType)"
        s.execute(query)
        s.close()
        c.close()
      case _ => List.empty[PlanInterval]
    }
  }
  def insertInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int): Unit = {
    val now = new Date(from)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    val nextHourNoPlan = nextHour(nowStart)

    val hours = ListBuffer.empty[Long]
    var h = nextHourNoPlan
    hours += h
    while (hours.length < hoursAmount) {
      h = nextHour(h)
      hours += h
    }

    //todo split existing task

    //todo move all tasks right > start_date

    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType)"
        s.execute(query)
        s.close()
        c.close()
      case _ => List.empty[PlanInterval]
    }


  }
  private def splitInterval(userId: Int, dateStart: Long, dateFinish: Long): Unit = {
    val hours = getHoursOfInterval(dateStart, dateFinish)

  }
  private def splitTask(splitDate: Long): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where date_start > $splitDate and date_finish <= $splitDate"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
          )
        }).toList

        if (plan.nonEmpty){
          val split = plan.head
          var splitFirst = split.date_start
          var splitSecond = splitDate
          while (splitFirst < splitSecond){

          }
        }

        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }
  private def intersectsPlan(hours: List[Long], plan: List[PlanInterval]): Int = {
    hours.count(h => plan.exists(p => p.date_start <= h && h <= p.date_finish))
  }
  private def getUserPlan(userId: Int, from: Long): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where user_id = $userId and date_finish >= $from"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
          )
        }).toList
        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }
  private def nextHourLatest(userId: Int, dateStart: Long): Long = {
    val now = new Date(dateStart)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    val nextHourNoPlan = nextHour(nowStart)
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where date_finish >= $dateStart order by date_finish desc limit 1"
        val latest = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
          )
        }).toList
        s.close()
        c.close()
        if (latest.nonEmpty){
          nextHour(latest.head.date_finish)
        }
        else{
          nextHourNoPlan
        }
      case _ => nextHourNoPlan
    }
  }
  private def moveIntervalRight(interval: PlanInterval, hours: Int, intervals: List[PlanInterval]): Unit = {
//    val newInterval = interval.copy(date_start = getNextHourN(interval.date_start, hours), date_finish = getNextHourN(interval.date_finish, hours))
//    val intervalHours = getHoursOfInterval(newInterval.date_start, newInterval.date_finish)
//    val notFree = intervalHours.filter(x => !isHourFree(x, intervals))
//    if (notFree.nonEmpty){
//      //moveIntervalRight()
//    }
  }
  private def getHoursOfInterval(dateStart: Long, dateFinish: Long): List[Long] = {
    val res = ListBuffer.empty[Long]
    var hour = dateStart
    res += hour
    while (hour < dateFinish){
      hour = nextHour(hour)
      res += hour
    }
    res.toList
  }
  private def nextHourN(date: Long, hours: Int): Long = {
    var h = nextHour(date)
    (2 to(hours)).foreach(_ => {
      h = nextHour(h)
    })
    h
  }
  private def nextFreeHour(date: Long, intervals: List[PlanInterval] = List.empty[PlanInterval]): Long = {
    var nH = nextHour(date)
    while (!isHourFree(date, intervals)) {
      nH = nextHour(nH)
    }
    nH
  }
  private def nextHour(date: Long): Long = {
    var d = new Date(date + msOneHour)
    val hours = d.getHours
    if (hours == 12){
      d = new Date(date + msOneHour * 2)
    }
    else if (isShort(d) && hours == 17) {
      d = new Date(date + msOneHour * 17)
    }
    else if (hours == 18){
      d = new Date(date + msOneHour * 16)
    }
    while (isWeekend(d)){
      d = new Date(d.getTime + msOneHour * 24)
    }
    new Date(d.getYear, d.getMonth, d.getDate, d.getHours, 0, 0).getTime
  }
  private def isHourFree(hour: Long, intervals: List[PlanInterval]): Boolean = {
    intervals.exists(x => x.date_start <= hour || hour <= x.date_finish)
  }
  private def sameDay(date1: Long, date2: Long): Unit = {
    val d1 = new Date(date1)
    val d2 = new Date(date2)
    d1.getYear == d2.getYear && d1.getMonth == d2.getMonth && d1.getDate == d2.getDate
  }
  private def isWeekend(d: Date): Boolean = {
    specialDays.find(x => x.day == d.getDate && x.month == d.getMonth && x.year == d.getYear) match {
      case Some(spec) => spec.kind == "weekend"
      case _ => d.getDay == 0 || d.getDay == 6
    }
  }
  private def isShort(d: Date): Boolean = {
    specialDays.find(x => x.day == d.getDate && x.month == d.getMonth && x.year == d.getYear) match {
      case Some(spec) => spec.kind == "short"
      case _ => false
    }
  }
  private def printDate(date: Long): Unit = {
    val d = new Date(date)
    println(d.getHours + " " + d.getDate + "/" + (d.getMonth + 1))
  }
}
