package deepsea.time

import akka.protobufv3.internal.Empty
import deepsea.actors.ActorManager
import deepsea.auth.AuthManagerHelper
import deepsea.database.DBManager
import deepsea.database.DBManager.{RsIterator, check}
import deepsea.issues.IssueManager.UpdateDates
import deepsea.time.PlanHoursManager.{PlanHour, SpecialDay}
import deepsea.time.PlanManager._
import deepsea.time.TimeControlManager.TimeControlInterval

import java.time.YearMonth
import java.util.{Calendar, Date}
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
  def getInterval(id: Int): List[PlanInterval] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"select * from plan where id = $id"
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
    val cal = Calendar.getInstance()
    cal.setTime(new Date(dateLong))
    val month = cal.get(Calendar.MONTH)
    val year = cal.get(Calendar.YEAR)
    val daysInMonth = cal.getActualMaximum(Calendar.DAY_OF_MONTH)

    getUsers.foreach(user => {
      val planByDays = ListBuffer.empty[PlanByDays]
      val skip = skipIntervals(user)
      1.to(daysInMonth).foreach(d => {
        val dayIntervals = ListBuffer.empty[DayInterval]
        cal.set(year, month, d)
        val hours = hoursOfDay(cal.getTime.getTime)
        if (hours.nonEmpty){
          val intervals = plan.filter(x => x.user_id == user && intervalSameDay(hours.head, hours.last, x.date_start, x.date_finish))
          intervals.foreach(int => {
            val intervalHours = hours.filter(x => int.date_start <= x && x <= int.date_finish).filter(h => int.task_type != 0 || !inInterval(h, skip))
            dayIntervals += DayInterval(int.task_id, intervalHours.length, int.hours_amount, int.id, int.date_start, int.consumed, int.task_type)
          })
          planByDays += PlanByDays(d, month, year, dayIntervals.sortBy(_.date_start).toList)
        }
      })
      res += UserPlan(user, planByDays.toList)
    })

    res.toList
  }
  def getIssuesByChunk(ids: List[Int], plan: List[PlanInterval]): List[IssuePlan] = {
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
  def inInterval(d1: Long, d2: Long, int1: Long, int2: Long): Boolean = {
    d1 <= int1 && int2 <= d2
  }
  def addInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int): Int = {
    if (taskType != 0){
      insertInterval(taskId, userId, from, hoursAmount, taskType)
    }
    else{
      val today = new Date()
      val todayStart = new Date(today.getYear, today.getMonth, today.getDate, 0, 0, 0).getTime
      if (from < todayStart) {
        -1
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
        val id = DBManager.GetPGConnection() match {
          case Some(c) =>
            val s = c.createStatement()
            val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType, ${hours.length}) returning id"
            val rs = s.executeQuery(query)
            val idNext = if (rs.next()) {
              rs.getInt("id")
            }
            else {
              -2
            }
            s.close()
            c.close()
            idNext
          case _ => -2
        }
        id
      }
    }
  }
  def addIntervalManual(taskId: Int, userId: Int, from: Long, to: Long, hoursAmount: Int, taskType: Int, consumed: Int): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount, consumed) values ($taskId, $userId, ${from}, ${to}, $taskType, ${hoursAmount}, ${consumed})"
        s.execute(query)
        s.close()
        c.close()
      case _ => None
    }
  }

  def deleteInterval(id: Int): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val intList = getInterval(id)
        if (intList.nonEmpty && intList.head.consumed == 0){
          val int = intList.head
          val today = new Date().getTime
          if (int.date_finish < today){
            s.execute(s"delete from plan where id = $id")
          }
          else{
            if (int.task_type != 0) {
              splitTask(int.date_start, int.user_id, int.hours_amount)
            }
            val userPlan = getUserPlan(int.user_id, int.date_start).filter(x => x.id != int.id).filter(_.task_type == 0)
            var hourStart = int.date_start
            userPlan.sortBy(_.date_start).foreach(p => {
              val hourFinish = nextHourN(hourStart, p.hours_amount - 1)
              s.execute(s"update plan set date_start = $hourStart, date_finish = $hourFinish where id = ${p.id}")
              hourStart = nextHour(hourFinish)
            })
            s.execute(s"delete from plan where id = $id")
          }
        }
        s.close()
        c.close()
      case _ => None
    }
  }
  def deletePausedInterval(id: Int): Unit = {
    val tasks = getTaskPlan(id).sortBy(_.date_start)
    tasks.findLast(_.consumed == 1) match {
      case Some(value) =>
        tasks.filter(x => x.date_start > value.date_finish).foreach(int => {
          deleteInterval(int.id)
        })
      case _ =>
        tasks.foreach(int => {
          deleteInterval(int.id)
        })
    }
  }
  def userLogin(id: Int): String = {
    val login = DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select login from users where id = $id")
        val loginValue = if (rs.next()){
          rs.getString("login")
        }
        else{
          "NOT FOUND"
        }
        s.close()
        c.close()
        loginValue
      case _ => ""
    }
    login
  }
  def insertInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int, consumed: Int = 0, min: Long = 0): Int = {
    val now = new Date(from)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    var nextHourNoPlan = nextHour(nowStart)
    while (min != 0 && nextHourNoPlan < min) {
      nextHourNoPlan = nextHour(nextHourNoPlan)
    }
    val todayStart = new Date(now.getYear, now.getMonth, now.getDate, 0, 0, 0)
    val plan = getUserPlan(userId, nextHourNoPlan).filter(_.consumed == 1)
    if (from < todayStart.getTime) {
      -1
    }
//    else if (plan.filter(x => sameDay(x.date_start, nextHourNoPlan)).map(_.hours_amount).sum > 0){
//      -2
//    }
    else {
      val skip = skipIntervals(userId)
      while (plan.exists(x => x.date_start <= nextHourNoPlan && nextHourNoPlan <= x.date_finish)) {
        nextHourNoPlan = nextHour(nextHourNoPlan)
      }
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

      val id = DBManager.GetPGConnection() match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"insert into plan (task_id, user_id, date_start, date_finish, task_type, hours_amount, consumed) values ($taskId, $userId, ${hours.head}, ${hours.last}, $taskType, ${hours.length}, $consumed) returning id"
          val rs = s.executeQuery(query)
          val idNext = if (rs.next()){
            rs.getInt("id")
          }
          else{
            0
          }
          s.close()
          c.close()
          idNext
        case _ => -3
      }
      id
    }
  }
  def insertConsumedInterval(taskId: Int, userId: Int, from: Long, hoursAmount: Int, taskType: Int): String = {
    val now = new Date(from)
    val nowStart = new Date(now.getYear, now.getMonth, now.getDate, 8, 0, 0).getTime
    val nextHourNoPlan = nextHour(nowStart)
    val planFull = getUserPlan(userId, 0)
    val plan = planFull.filter(_.date_start >= nextHourNoPlan)
    val tasks = getIssue(taskId)
    if (tasks.isEmpty){
      "error: consumed task not found"
    }
    else{
      val task = tasks.head
      if (plan.exists(x => x.consumed == 1 && !sameDay(x.date_start, nextHourNoPlan))) {
        "error: there are some consumed hours in next days"
      }
      else if (plan.filter(x => x.consumed == 1 && sameDay(x.date_start, nextHourNoPlan)).map(_.hours_amount).sum + hoursAmount > 8) {
        "error: not enough work hours this day"
      }
      else if ((task.consumed + hoursAmount) > task.plan) {
        "error: not enough task plan hours"
      }
      else {
        val ins = insertInterval(taskId, userId, from, hoursAmount, taskType, 1)
        val plan = getInterval(ins)
        if (plan.nonEmpty){
          val int = plan.head
          val insPlan = getUserPlan(userId, nextHourNoPlan).filter(_.consumed == 0).filter(_.task_id == taskId).filter(_.date_start > int.date_finish)
          var hoursConsumed = 0
          if (ins > 0) {
            if (insPlan.nonEmpty) {
              insPlan.foreach(cTask => {
                if (hoursConsumed < hoursAmount) {
                  if (cTask.hours_amount <= hoursAmount) {
                    hoursConsumed += cTask.hours_amount
                    deleteInterval(cTask.id)
                  }
                  else if (cTask.hours_amount > hoursAmount) {
                    val tasks = getInterval(cTask.id)
                    if (tasks.nonEmpty){
                      val task = tasks.head
                      val minDate = if (int.date_finish < task.date_start){
                        task.date_start
                      }
                      else{
                        int.date_finish
                      }
                      deleteInterval(cTask.id)
                      insertInterval(taskId, userId, cTask.date_start, cTask.hours_amount + hoursConsumed - hoursAmount, cTask.task_type, min = minDate)
                      hoursConsumed += cTask.hours_amount
                    }
                  }
                }
              })
            }
          }
          "success"
        }
        else{
          "error: could not insert new interval"
        }
      }
    }
  }

  def getUserPlanHours(userId: Int, startDate: Long = 0, amount: Int = 25, available: Boolean = false): List[PlanHour] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val calendar = Calendar.getInstance()
        if (startDate != 0) {
          calendar.setTime(new Date(startDate))
        }
        else {
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
        val query = s"select * from plan where date_start >= $splitDate and user_id = $userId and task_type = 0"
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
          val nonConsumed = plan.filter(_.consumed == 0)
          val beforeStartReduce = if (nonConsumed.nonEmpty){
            val planMin = nonConsumed.map(_.date_start).min
            val hoursBeforeStart = ListBuffer.empty[Long]
            var startDate = splitDate
            while (startDate < planMin) {
              hoursBeforeStart += startDate
              startDate = nextHour(startDate)
            }
            if (amount < hoursBeforeStart.length) {
              0
            }
            else {
              amount - hoursBeforeStart.length
            }
          }
          else{
            amount
          }


          plan.sortBy(_.date_start).foreach(p => {
            val pN = p.copy(date_start = nextHourN(p.date_start, beforeStartReduce), date_finish = nextHourN(p.date_finish, beforeStartReduce))
            s.execute(s"update plan set date_start = ${pN.date_start}, date_finish = ${pN.date_finish} where id = ${p.id}")

            getInterval(pN.id).headOption match {
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
                ActorManager.issue ! UpdateDates(int.task_id, dateStart, dateFinish)
              case _ => None
            }

          })

        }



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
        val query = s"select * from plan where date_start < $splitDate and date_finish >= $splitDate and user_id = $userId and task_type = 0 and consumed = 0"
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
  def nextHour(date: Long): Long = {
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
  def sameDay(date1: Long, date2: Long): Boolean = {
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


  def getUserStats(dateFrom: Long, dateTo: Long, userIds: List[Int]): List[UserStats] = {

    val res = ListBuffer.empty[UserStats]

    val calendarFrom = Calendar.getInstance()
    calendarFrom.setTime(new Date(dateFrom))
    calendarFrom.set(calendarFrom.get(Calendar.YEAR), calendarFrom.get(Calendar.MONTH), calendarFrom.get(Calendar.DAY_OF_MONTH), 8, 0, 0)
    val calendarFromDate = calendarFrom.getTime.getTime

    val calendarTo = Calendar.getInstance()
    calendarTo.setTime(new Date(dateTo))
    calendarTo.set(calendarTo.get(Calendar.YEAR), calendarTo.get(Calendar.MONTH), calendarTo.get(Calendar.DAY_OF_MONTH), 23, 0, 0)
    val calendarToDate = calendarTo.getTime.getTime

    val planByDays = getPlanByDays(calendarFromDate)
    val issues = getIssuesByChunk(planByDays.flatMap(_.plan).flatMap(_.ints).map(_.taskId).distinct, List.empty[PlanInterval])
    val tcUsers = getTCUsers.filter(x => userIds.contains(x.id))

    val planHours = getHoursOfInterval(calendarFromDate, calendarToDate)
    val dmys = dmyFromHours(planHours)
    val planCalendar = specialDays.filter(x => dmys.exists(y => y.day == x.day && y.month == x.month - 1 && y.year == y.year))
      .map(_.kind match {
        case "short" => 7
        case "weekend" => 0
      }).sum + dmys.length * 8


    val usersTCHours = tcUsers.filter(x => userIds.contains(x.id)).flatMap(u => getUserTimeControl(u.tcid.toString, calendarFromDate))
    tcUsers.foreach(tcUser => {
      val details = ListBuffer.empty[UserStatsDetails]
      val planByDaysPeriod = ListBuffer.empty[DayInterval]
      planByDays.find(_.userId == tcUser.id) match {
        case Some(userPlan) =>
          dmys.foreach(dmy => {
            val calendar = Calendar.getInstance()
            calendar.set(dmy.year, dmy.month, dmy.day, 9, 0)
            val longDate = calendar.getTime.getTime
            val strDate = stringDate(dmy.day, dmy.month, dmy.year)
            val tasks = ListBuffer.empty[UserStatsDetailsTask]
            val officeIntervals = usersTCHours.filter(_.userId == tcUser.tcid.toString).filter(x => sameDay(calendar.getTime.getTime, x.startDate))
            val officeTime = officeIntervals.map(x => x.endTime - x.startTime).sum / (1000 * 60 * 60).toDouble

            userPlan.plan.find(x => x.day == dmy.day && x.month == dmy.month && x.year == dmy.year) match {
              case Some(pl) =>
                planByDaysPeriod += pl.ints
                pl.ints.filter(_.consumed == 1).filter(_.taskType == 0).foreach(int => {
                  issues.find(_.id == int.taskId) match {
                    case Some(issue) =>
                      tasks += UserStatsDetailsTask(
                        issue.id,
                        issue.issue_type,
                        issue.name,
                        issue.docNumber,
                        int.hours
                      )
                    case _ => None
                  }
                })
              case _ => None
            }

            details += UserStatsDetails(
              longDate,
              strDate,
              officeTime,
              stringTime(officeTime),
              tasks.toList
            )
          })
        case _ => None
      }


      val vacation = Math.ceil(planByDaysPeriod.filter(_.taskType == 2).map(_.hours).sum / 8).toInt
      val medical = Math.ceil(planByDaysPeriod.filter(_.taskType == 1).map(_.hours).sum / 8).toInt
      val dayOff = Math.ceil(planByDaysPeriod.filter(_.taskType == 4).map(_.hours).sum / 8).toInt
      val study = planByDaysPeriod.filter(_.taskType == 4).map(_.hours).sum

      res += UserStats(
        tcUser.id,
        tcUser.tcid,
        planCalendar,
        Math.round(details.map(_.officeTime).sum).toInt,
        details.flatMap(_.tasks).map(_.hours).sum,
        vacation,
        medical,
        dayOff,
        study,
        details.toList
      )
    })

    res.toList
  }

  def dmyFromHours(hours: List[Long]): List[DMY] = {
    val res = ListBuffer.empty[DMY]
    val c = Calendar.getInstance()
    hours.map(h => {
      c.setTime(new Date(h))
      val d = c.get(Calendar.DAY_OF_MONTH)
      val m = c.get(Calendar.MONTH)
      val y = c.get(Calendar.YEAR)
      res.find(x => x.day == d && x.month == m && x.year == y) match {
        case Some(value) => None
        case _ => res += DMY(d, m, y)
      }
    })
    res.sortBy(x => x.year + "-" + x.month + "-" + x.day).toList
  }
  def stringTime(hoursValue: Double): String = {
    val hours = Math.floor(hoursValue).toInt
    val minutes = Math.floor((hoursValue - hours) * 60).toInt
    leftZeros(hours.toString) + ":" + leftZeros(minutes.toString)
  }
  def stringDate(day: Int, month: Int, year: Int): String = {
    leftZeros(day.toString) + "/" + leftZeros((month + 1).toString) + "/" + year
  }
  def leftZeros(in: String, length: Int = 2): String = {
    var res = in
    while (res.length < length) {
      res = "0" + res
    }
    res
  }
  def getTCUsers: List[UserTCID] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val query = s"select id, tcid from users"
        val s = c.createStatement()
        val users = RsIterator(s.executeQuery(query)).map(rs => {
          UserTCID(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getInt("tcid")).getOrElse(0),
          )
        }).toList
        s.close()
        c.close()
        users
      case _ => List.empty[UserTCID]
    }
  }
  def getUserTimeControl(tcId: String, dateFrom: Long): ListBuffer[TimeControlInterval] = {
    val res = ListBuffer.empty[TimeControlInterval]
    DBManager.GetFireBaseConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val today = new Date().getTime
        val days = (today - dateFrom) / (1000 * 60 * 60 * 24) + 3
        val query = s"select * from GRAPH_FACT WHERE STARTDATE <= current_date + 1 and STARTDATE >= current_date - $days and uid = '$tcId'"
        val rs = s.executeQuery(query)
        while (rs.next()) {
          val closeDoor = rs.getInt("CLOSEDOOR") match {
            case value: Int => value
            case _ => 0
          }
          val addDoor = rs.getInt("ADDDOOR") match {
            case value: Int => value
            case _ => 0
          }
          res += TimeControlInterval(
            rs.getString("UID") match {
              case userId: String => userId
              case _ => ""
            },
            rs.getDate("STARTTIME") match {
              case date: Date => date.getTime
              case _ => 0
            },
            rs.getDate("ENDTIME") match {
              case date: Date =>
                if (addDoor != 0 && closeDoor == 0){
                  new Date().getTime
                }
                else{
                  date.getTime
                }
              case _ => 0
            },
            rs.getDate("STARTDATE") match {
              case date: Date => date.getTime
              case _ => 0
            },
            rs.getDate("ENDDATE") match {
              case date: Date => date.getTime
              case _ => 0
            },
            addDoor,
            closeDoor
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res
  }
}
