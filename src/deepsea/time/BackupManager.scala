package deepsea.time

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager
import deepsea.mail.MailManager.Mail
import deepsea.time.BackupManager.BackupForan
import deepsea.time.TimeAndWeatherManager.{SetTimeAndWeather, Weather}

import java.util.{Calendar, Date}
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt
import scala.io.Source

object BackupManager{
  case class BackupForan()
}
class BackupManager extends Actor{
  val foranProjects: List[String] = List("P701", "P707", "N002", "N004")
  val procedure: String = Source.fromResource("queries/backupForan.sql").mkString

  implicit val system: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val executor: ExecutionContextExecutor = system.dispatcher

  override def preStart(): Unit = {
    system.scheduler.scheduleWithFixedDelay(0.seconds, 60.minutes, self, BackupForan())
  }
  override def receive: Receive = {
    case BackupForan() =>
      if (Calendar.getInstance().get(Calendar.HOUR_OF_DAY) == 2){
        val start = new Date().toString
        backupForan()
        val complete = new Date().toString
        ActorManager.mail ! Mail("Bogdan Isaev", "redeeming.fury@gmail.com", "Foran Backup Notification", s"Foran Backup started at $start and successfully completed at $complete")
      }
  }
  def backupForan(): Unit ={
    val configOracle = new HikariConfig()
    configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
    configOracle.setUsername("SYS AS SYSDBA")
    configOracle.setPassword("Whatab0utus")
    configOracle.setMaximumPoolSize(1)
    configOracle.setAutoCommit(true)
    val dsOracle = new HikariDataSource(configOracle)
    val c = dsOracle.getConnection
    val s = c.createStatement()
    val jobName = "NAUTICBACKUP-" + new Date().getTime
    val q = "'"
    val query = procedure.replaceAll("&jobname", jobName).replaceAll("&projects", foranProjects.map(x => q + x + q).mkString(","))
    s.execute(query)
    s.execute("BEGIN\n    -- Call\n    SYS.NAUTIC_BACKUP;\n\n    -- Transaction Control\n    COMMIT;\nEND;")
    s.close()
    c.close()
  }
}
