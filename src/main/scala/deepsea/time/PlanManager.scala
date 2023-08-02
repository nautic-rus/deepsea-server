package deepsea.time

import akka.actor.Actor

import java.util.Date

object PlanManager{
  case class PlanInterval(id: Long, task_id: Int, user_id: Int, date_start: Long, date_finish: Long, task_type: Int)
}
class PlanManager extends Actor with PlanManagerHelper {
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
    case _ => None
  }
}
