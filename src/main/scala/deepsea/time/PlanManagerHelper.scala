package deepsea.time

import deepsea.auth.AuthManagerHelper
import deepsea.database.DBManager
import deepsea.database.DBManager.RsIterator
import deepsea.time.PlanHoursManager.SpecialDay
import deepsea.time.PlanManager.{DayInterval, IssuePlan, PlanByDays, PlanInterval, UserPlan}

import java.time.YearMonth
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.io.Source

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
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList
        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }
  def getTaskPlan(id: Int): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where task_id = $id"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList
        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }

  def getUserPlan(userId: Int, from: Long): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where user_id = $userId and date_start >= $from"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList
        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }
  def getPlanByDays(dateLong: Long): List[UserPlan] = {
    val plan = getPlan
    val res = ListBuffer.empty[UserPlan]
    val date = new Date(dateLong)
    val month = date.getMonth
    val year = date.getYear
    val daysInMonth = YearMonth.of(date.getYear, date.getMonth).lengthOfMonth()

    getUsers.foreach(user => {
      val planByDays = ListBuffer.empty[PlanByDays]
      val skip = skipIntervals(user)
      1.to(daysInMonth).foreach(d => {
        val dayIntervals = ListBuffer.empty[DayInterval]
        val dayDate = new Date(year, month, d)
        val hours = hoursOfDay(dayDate.getTime)
        if (hours.nonEmpty){
          val intervals = plan.filter(x => x.user_id == user && intervalSameDay(hours.head, hours.last, x.date_start, x.date_finish))
          intervals.foreach(int => {
            val intervalHours = hours.filter(x => int.date_start <= x && x <= int.date_finish).filter(h => int.task_type != 0 || !inInterval(h, skip))
            dayIntervals += DayInterval(int.task_id, intervalHours.length, int.hours_amount, int.id, int.date_start)
          })
          planByDays += PlanByDays(d, month, year, dayIntervals.sortBy(_.date_start).toList)
        }
      })
      res += UserPlan(user, planByDays.toList)
    })

    res.toList
  }
  def getIssuesAux(ids: List[Int], plan: List[PlanInterval]): List[IssuePlan] = {
    val res = ListBuffer.empty[IssuePlan]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        ids.grouped(900).foreach(group => {
          val query = Source.fromResource("queries/plan-issues.sql").mkString + s" and id in (${ids.mkString(",")})"
          val rSet = RsIterator(s.executeQuery(query))
          res ++= rSet.map(rs => {
            val id = rs.getInt("id")
            val consumed = plan.filter(_.task_id == id).filter(_.consumed == 1).map(_.hours_amount).sum
            val planHours = rs.getInt("plan_hours")
            val inPlan = plan.filter(_.task_id == id).map(_.hours_amount).sum
            val available = planHours - inPlan
            IssuePlan(
              id,
              rs.getString("issue_name"),
              rs.getString("doc_number"),
              planHours,
              rs.getString("status"),
              rs.getString("issue_type"),
              rs.getString("period"),
              rs.getString("assigned_to"),
              rs.getString("project"),
              rs.getString("department"),
              Option(rs.getString("closing_status")).getOrElse(""),
              Option(rs.getLong("stage_date")).getOrElse(0),
              consumed, inPlan, available, available
            )
          }).toList
          rSet.rs.close()
        })
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }
  def getIssues: List[IssuePlan] = {
    val res = ListBuffer.empty[IssuePlan]
    val plan = getPlan
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/plan-issues.sql").mkString
        val rSet = RsIterator(s.executeQuery(query))
        res ++= rSet.map(rs => {
          val id = rs.getInt("id")
          val consumed = plan.filter(_.task_id == id).filter(_.consumed == 1).map(_.hours_amount).sum
          val planHours = rs.getInt("plan_hours")
          val inPlan = plan.filter(_.task_id == id).map(_.hours_amount).sum
          val available = planHours - inPlan
          val docNumber = rs.getString("doc_number").trim
          val docName = rs.getString("issue_name").trim
          val docNameTrim = if (docName.length > 50) {
            docName.toLowerCase.take(50) + "..."
          }
          else {
            docName.toLowerCase
          }
          val docNumberTrim = if (docNumber != "") docNumber else "Без номера"
          IssuePlan(
            id,
            docName,
            docNumber,
            planHours,
            rs.getString("status"),
            rs.getString("issue_type"),
            rs.getString("period"),
            rs.getString("assigned_to"),
            rs.getString("project"),
            rs.getString("department"),
            Option(rs.getString("closing_status")).getOrElse(""),
            Option(rs.getLong("stage_date")).getOrElse(0),
            consumed, inPlan, available, available
          )
        }).toList
        rSet.rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }
  def getIssue(id: Int): List[IssuePlan] = {
    val res = ListBuffer.empty[IssuePlan]
    val plan = getTaskPlan(id)
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/plan-issues.sql").mkString + s" and id = $id"
        val rSet = RsIterator(s.executeQuery(query))
        res ++= rSet.map(rs => {
          val id = rs.getInt("id")
          val consumed = plan.filter(_.task_id == id).filter(_.consumed == 1).map(_.hours_amount).sum
          val planHours = rs.getInt("plan_hours")
          val inPlan = plan.filter(_.task_id == id).map(_.hours_amount).sum
          val available = planHours - inPlan
          IssuePlan(
            id,
            rs.getString("issue_name").trim,
            rs.getString("doc_number").trim,
            planHours,
            rs.getString("status"),
            rs.getString("issue_type"),
            rs.getString("period"),
            rs.getString("assigned_to"),
            rs.getString("project"),
            rs.getString("department"),
            Option(rs.getString("closing_status")).getOrElse(""),
            Option(rs.getLong("stage_date")).getOrElse(0),
            consumed, inPlan, available, available
          )
        }).toList
        rSet.rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }

  def getUsers: List[Int] = {
    val res = ListBuffer.empty[Int]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        try {
          val s = c.createStatement();
          val rs = s.executeQuery(s"select id from users where removed = 0 order by id")
          while (rs.next()) {
            res += Option(rs.getInt("id")).getOrElse(0)
          }
          rs.close()
          s.close()
          c.close()
        } catch {
          case e: Exception => println(e.toString)
        }
      case _ =>
    }
    res.toList
  }
  def intervalSameDay(d1: Long, d2: Long, int1: Long, int2: Long): Boolean = {
    val c1 = int2 >= d1 && int1 <= d1
    val c2 = int1 >= d1 && int1 <= d2 && int2 <= d2 && int2 >= d1
    val c3 = int1 >= d1 && int1 <= d2 && int2 >= d2
    c1 || c2 || c3
  }
  def addInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int): String = {
    if (taskType != 0){
      insertInterval(taskId, userId, from, hoursAmount, taskType)
    }
    else{
      val today = new Date()
      val todayStart = new Date(today.getYear, today.getMonth, today.getDate, 0, 0, 0).getTime
      if (from < todayStart) {
        "error: wrong planning date"
      }
      else {
        val skip = skipIntervals(userId)
        val dateFrom = nextHourLatest(userId, todayStart)
        val hours = ListBuffer.empty[Long]
        var h = dateFrom
        while (hours.length < hoursAmount) {
          if (!inInterval(h, skip)) {
            hours += h
          }
          h = nextHour(h)
        }
        DBManager.GetPGConnection() match {
          case Some(c) =>
            val s = c.createStatement()
            val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType, ${hours.length})"
            s.execute(query)
            s.close()
            c.close()
          case _ => List.empty[PlanInterval]
        }
        "success"
      }
    }
  }
  def deleteInterval(id: Int): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val intList = RsIterator(s.executeQuery(s"select * from plan where id = $id")).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList

        if (intList.nonEmpty){
          val int = intList.head
          if (int.task_type != 0){
            splitTask(int.date_start, int.user_id, int.hours_amount)
          }
          val userPlan = getUserPlan(int.user_id, int.date_start).filter(x => x.id != int.id)
          var hourStart = int.date_start
          userPlan.sortBy(_.date_start).foreach(p => {
            val hourFinish = nextHourN(hourStart, p.hours_amount - 1)
            s.execute(s"update plan set date_start = $hourStart, date_finish = $hourFinish where id = ${p.id}")
            hourStart = nextHour(hourFinish)
          })

          s.execute(s"delete from plan where id = $id")
        }

        s.close()
        c.close()
      case _ => None
    }
  }
  def insertInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int, consumed: Int = 0): String = {
    val now = new Date(from)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    val nextHourNoPlan = nextHour(nowStart)

    val todayStart = new Date(now.getYear, now.getMonth, now.getDate, 0, 0, 0)
    if (from < todayStart.getTime) {
      "error: wrong planning date"
    }
    else {
      val skip = skipIntervals(userId)
      val hours = ListBuffer.empty[Long]
      var h = nextHourNoPlan
      while (hours.length < hoursAmount) {
        if (!inInterval(h, skip)) {
          hours += h
        }
        h = nextHour(h)
      }

      splitTask(nextHourNoPlan, userId)

      moveRight(nextHourNoPlan, userId, hours.length)

      DBManager.GetPGConnection() match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount, consumed) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType, ${hours.length}, $consumed)"
          s.execute(query)
          s.close()
          c.close()
        case _ => List.empty[PlanInterval]
      }
      "success"
    }
  }

  def insertConsumedInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int): String = {
    val now = new Date(from)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    val nextHourNoPlan = nextHour(nowStart)

    val todayStart = new Date(now.getYear, now.getMonth, now.getDate, 0, 0, 0)
    if (from < todayStart.getTime) {
      "error: wrong planning date"
    }
    else {
      val skip = skipIntervals(userId)
      val hours = ListBuffer.empty[Long]
      var h = nextHourNoPlan
      while (hours.length < hoursAmount) {
        if (!inInterval(h, skip)) {
          hours += h
        }
        h = nextHour(h)
      }

      splitTask(nextHourNoPlan, userId)

      moveRight(nextHourNoPlan, userId, hours.length)

      DBManager.GetPGConnection() match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType, ${hours.length})"
          s.execute(query)
          s.close()
          c.close()
        case _ => List.empty[PlanInterval]
      }
      "success"
    }
  }

  private def skipIntervals(userId: Int): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where task_type != 0 and user_id = $userId"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList
        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }
  private def moveRight(splitDate: Long, userId: Int, amount: Int): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where date_start >= $splitDate and user_id = $userId"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList

        plan.sortBy(_.date_start).foreach(p => {
          val pN = p.copy(date_start = nextHourN(p.date_start, amount), date_finish = nextHourN(p.date_finish, amount))
          s.execute(s"update plan set date_start = ${pN.date_start}, date_finish = ${pN.date_finish} where id = ${p.id}")
        })


        s.close()
        c.close()
        plan
      case _ => List.empty[PlanInterval]
    }
  }
  private def splitTask(splitDate: Long, userId: Int, removeAmount: Int = 0): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where date_start < $splitDate and date_finish >= $splitDate and user_id = $userId and task_type = 0"
        val plan = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
          )
        }).toList

        if (plan.nonEmpty){
          val split = plan.head
          val splitFirstStart = split.date_start
          var splitFirstFinish = split.date_start
          val splitSecondStart = splitDate
          val splitSecondFinish = split.date_finish

          val splitFirstHours = ListBuffer.empty[Long]
          splitFirstHours += splitFirstFinish
          while (splitFirstFinish < splitDate){
            splitFirstFinish = nextHour(splitFirstFinish)
            splitFirstHours += splitFirstFinish
          }

          val split1 = split.copy(date_start = splitFirstStart, date_finish = splitFirstHours.dropRight(1).last)
          val split2 = split.copy(date_start = splitSecondStart, date_finish = nextHourN(splitSecondStart, getHoursOfInterval(splitSecondStart, splitSecondFinish).length - removeAmount - 1))

          DBManager.GetPGConnection() match {
            case Some(c) =>
              val s = c.createStatement()
              val qRemove = s"delete from plan where id = ${split.id}"
              s.execute(qRemove)
              val q1 = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount) values (${split.task_id}, ${split.user_id}, ${split1.date_start}, ${split1.date_finish}, ${split.task_type}, ${getHoursOfInterval(split1.date_start, split1.date_finish).length})"
              s.execute(q1)
              val q2 = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount) values (${split.task_id}, ${split.user_id}, ${split2.date_start}, ${split2.date_finish}, ${split.task_type}, ${getHoursOfInterval(split2.date_start, split2.date_finish).length})"
              s.execute(q2)
              s.close()
              c.close()
            case _ => None
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
  private def nextHourLatest(userId: Int, dateStart: Long): Long = {
    val now = new Date(dateStart)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    val nextHourNoPlan = nextHour(nowStart)
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where task_type = 0 and date_finish >= $dateStart and user_id = $userId order by date_finish desc limit 1"
        val latest = RsIterator(s.executeQuery(query)).map(rs => {
          PlanInterval(
            rs.getInt("id"),
            rs.getInt("task_id"),
            rs.getInt("user_id"),
            rs.getLong("date_start"),
            rs.getLong("date_finish"),
            rs.getInt("task_type"),
            rs.getInt("hours_amount"),
            rs.getInt("consumed"),
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
  private def getHoursOfInterval(dateStart: Long, dateFinish: Long): List[Long] = {
    val res = ListBuffer.empty[Long]
    var hour = dateStart
    while (hour <= dateFinish){
      res += hour
      hour = nextHour(hour)
    }
    res.toList
  }
  private def nextHourN(date: Long, hours: Int): Long = {
    hours match {
      case 0 => date
      case 1 => nextHour(date)
      case _ =>
        var h = nextHour(date)
        (2 to (hours)).foreach(_ => {
          h = nextHour(h)
        })
        h
    }
  }
  private def nextFreeHour(date: Long, intervals: List[PlanInterval] = List.empty[PlanInterval]): Long = {
    var nH = nextHour(date)
    while (!inInterval(date, intervals)) {
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
  private def inInterval(hour: Long, intervals: List[PlanInterval]): Boolean = {
    intervals.exists(x => x.date_start <= hour && hour <= x.date_finish)
  }
  private def sameDay(date1: Long, date2: Long): Boolean = {
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
  private def hoursOfDay(dateStart: Long): List[Long] = {
    val res = ListBuffer.empty[Long]
    val now = new Date(dateStart)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    var nextHourNoPlan = nextHour(nowStart)
    while (sameDay(nextHourNoPlan, dateStart)){
      res += nextHourNoPlan
      nextHourNoPlan = nextHour(nextHourNoPlan)
    }
    res.toList
  }
}
