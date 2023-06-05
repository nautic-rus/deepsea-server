package deepsea.time

import akka.actor.{Actor, ActorSystem}
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.mail.MailManager.Mail
import deepsea.time.BackupManager.{BackupForan, NullHostKeyVerifier}
import net.schmizz.sshj.SSHClient
import net.schmizz.sshj.sftp.{RemoteResourceInfo, SFTPClient}
import net.schmizz.sshj.transport.verification.HostKeyVerifier
import org.aarboard.nextcloud.api.NextcloudConnector
import java.io.File
import java.nio.file.Files
import java.security.PublicKey
import java.time.LocalDate
import java.util
import java.util.{Calendar, Date}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt
import scala.io.Source



object BackupManager{
  case class BackupForan()
  class NullHostKeyVerifier() extends HostKeyVerifier {
    override def verify(s: String, i: Int, publicKey: PublicKey): Boolean = true

    override def findExistingAlgorithms(s: String, i: Int): util.List[String] = util.Arrays.asList()
  }
}
class BackupManager extends Actor{
  val foranProjects: List[String] = List("P701", "P707", "N002", "N003", "N004", "N005", "SC01", "LV01", "KA01", "AN01")
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
    configOracle.setDriverClassName("oracle.jdbc.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@192.168.1.12:1521:ORA3DB")
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
    dsOracle.close()


//    val cloud = new NextcloudConnector(App.Cloud.Host, true, 443, "admin", "c6bc9d23ab01701263b03e364ca5a32bacda46514c2d299e")
//    val dir = Files.createTempDirectory("download")
//    val sp = File.separator
//    val date = LocalDate.now().toString
//    val remoteDir = "/backups/foran/backup-" + date
//    cloud.createFolder(remoteDir)
//
//
//    val hostname = "192.168.1.30"
//    val username = "backupper"
//    val password = "IsntItEn0ugh"
//
//
//    val ssh = new SSHClient()
//    ssh.addHostKeyVerifier(new NullHostKeyVerifier())
//    ssh.connect(hostname)
//    ssh.authPassword(username, password)
//    val sftp: SFTPClient = ssh.newSFTPClient()
//    val ls = sftp.ls("/backups/oracle/")
//    val backups = ListBuffer.empty[RemoteResourceInfo]
//    ls.forEach(s => backups += s)
//    val sorted = backups.sortBy(_.getAttributes.getMtime).reverse.filter(_.getName.contains(".fdp")).take(foranProjects.length)
//    //val sorted = backups.sortBy(_.getAttributes.getMtime).reverse.filter(_.getName.contains(".fdp")).take(1)
//
//    sorted.foreach(s => {
//      val tempFile = new File(dir + sp + s.getName)
//      sftp.get(s.getPath, tempFile.getPath)
//      cloud.uploadFile(tempFile, remoteDir + "/" + s.getName)
//    })
//
//    sftp.close()
//    ssh.disconnect()
//
//
//    val hostnameSurf = "192.168.1.15"
//    val usernameSurf = "root"
//    val passwordSurf = "Whatab0utus"
//
//    val sshSurf = new SSHClient()
//    sshSurf.addHostKeyVerifier(new NullHostKeyVerifier())
//    sshSurf.connect(hostnameSurf)
//    sshSurf.authPassword(usernameSurf, passwordSurf)
//    val sftpSurf: SFTPClient = sshSurf.newSFTPClient()
//    val backupsSurf = ListBuffer.empty[RemoteResourceInfo]
//    val remoteSurfDir = "/foran/config/surfaces/"
//    sftpSurf.ls(remoteSurfDir).forEach(l => backupsSurf += l)
//
//    foranProjects.foreach(p => {
//      backupsSurf.find(_.getName.toLowerCase == p.toLowerCase) match {
//        case Some(surfDir) =>
//          val surfLs = ListBuffer.empty[RemoteResourceInfo]
//          sftpSurf.ls(surfDir.getPath).forEach(l => surfLs += l)
//          surfLs.filter(_.isRegularFile).filter(_.getName.split("\\.").last == "fsf").foreach(s => {
//            val tempFile = new File(dir + sp + s.getName)
//            sftpSurf.get(s.getPath, tempFile.getPath)
//            cloud.uploadFile(tempFile, remoteDir + "/" + s.getName)
//          })
//        case _ =>
//      }
//    })
//    sftpSurf.close()
//    sshSurf.disconnect()
//    dsOracle.close()


  }
}
