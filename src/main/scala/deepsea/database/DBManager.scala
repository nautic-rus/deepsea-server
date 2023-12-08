package deepsea.database

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.database.DatabaseManager.OracleConnection
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.sql.{Connection, ResultSet}
import scala.collection.mutable.ListBuffer

object DBManager extends MongoCodecs {

  case class RsIterator(rs: ResultSet) extends Iterator[ResultSet] {
    def hasNext: Boolean = rs.next()
    def next(): ResultSet = rs
  }

  private val configOracle = new HikariConfig()
  private val oracleConnections = ListBuffer.empty[OracleConnection]
  private val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.36")

//  JSch.setConfig("StrictHostKeyChecking", "no")
//  val nextcloudSSH = new JSch()
//  val nextcloudSSHSession: Session = nextcloudSSH.getSession("root", "89.108.125.21")
//  nextcloudSSHSession.setPassword("Whatab0utus")
//  nextcloudSSHSession.connect()
//  nextcloudSSHSession.setPortForwardingL("localhost", 1155, "172.18.0.5", 5432)


  List("N002", "N004", "N007", "SC01", "LV01").foreach(project => {
    try{
      configOracle.setDriverClassName("oracle.jdbc.OracleDriver")
      configOracle.setJdbcUrl("jdbc:oracle:thin:@192.168.1.12:1521:ORA3DB")
      configOracle.setUsername("C" + project)
      configOracle.setPassword("Whatab0utus")
      configOracle.setMaximumPoolSize(2)
      oracleConnections += OracleConnection(project, new HikariDataSource(configOracle))
    }
    catch {
      case e: Exception =>
    }
  })

  private val configPG = new HikariConfig()
  configPG.setDriverClassName("org.postgresql.Driver")
  configPG.setJdbcUrl("jdbc:postgresql://192.168.1.26/deepsea")
  configPG.setUsername("deepsea")
  configPG.setPassword("Ship1234")
  configPG.setMaximumPoolSize(20)
  val dsPG = new HikariDataSource(configPG)

  private val configPGFest = new HikariConfig()
  configPGFest.setDriverClassName("org.postgresql.Driver")
  configPGFest.setJdbcUrl("jdbc:postgresql://192.168.1.26/nautic_fest")
  configPGFest.setUsername("deepsea")
  configPGFest.setPassword("Ship1234")
  configPGFest.setMaximumPoolSize(5)
  val dsPGFest = new HikariDataSource(configPGFest)

//  private val configNextCloud = new HikariConfig()
//  configNextCloud.setDriverClassName("org.postgresql.Driver")
//  configNextCloud.setJdbcUrl("jdbc:postgresql://localhost:1155/nextcloud_database")
//  configNextCloud.setUsername("oc_nextcloud")
//  configNextCloud.setPassword("cab5a0e8b5db655ec2d8ce46efd22cea3f5b137ee450deb3")
//  configNextCloud.setMaximumPoolSize(25)
//  var dsNextCloud = new HikariDataSource(configNextCloud)

  private val configNextCloud = new HikariConfig()
  configNextCloud.setDriverClassName("org.postgresql.Driver")
  configNextCloud.setJdbcUrl("jdbc:postgresql://cloud.nautic-rus.com:5570/nextcloud_database")
  configNextCloud.setUsername("oc_nextcloud")
  configNextCloud.setPassword("cab5a0e8b5db655ec2d8ce46efd22cea3f5b137ee450deb3")
  configNextCloud.setMaximumPoolSize(5)
  var dsNextCloud = new HikariDataSource(configNextCloud)

  val configFireBase = new HikariConfig()
  configFireBase.setDriverClassName("org.firebirdsql.jdbc.FBDriver")
  configFireBase.setJdbcUrl("jdbc:firebirdsql://192.168.1.21:3053/C:/Program Files (x86)/TimeControl/BASE/OKO.FDB?charSet=UTF8")
  configFireBase.setUsername("MEGA")
  configFireBase.setPassword("STMEGA21")
  val dsFireBase = new HikariDataSource(configFireBase)


  def GetOracleConnection(project: String): Option[Connection] ={
    oracleConnections.find(_.project == project) match {
      case Some(connection) => Option(connection.ds.getConnection)
      case _ => Option.empty
    }
  }
  def GetPGConnection(): Option[Connection] = {
    Option(dsPG.getConnection)
  }
  def GetPGFestConnection(): Option[Connection] = {
    Option(dsPGFest.getConnection)
  }
  def GetMongoConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
  }
  def GetMongoCacheConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry))
  }
  def GetMongoFilesConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("files").withCodecRegistry(codecRegistry))
  }
  def GetNextCloudConnection(): Option[Connection] = {
    Option(dsNextCloud.getConnection)
  }
  def GetFireBaseConnection(): Option[Connection] = {
    Option(dsFireBase.getConnection)
  }
  def check(): String ={
    val dsPgStatus = "dsPg " + dsPG.getHikariPoolMXBean.getActiveConnections
    val dsFireBaseStatus = "dsFireBase " + dsFireBase.getHikariPoolMXBean.getActiveConnections
    val dsNextCloudStatus = "dsNextCloud " + dsNextCloud.getHikariPoolMXBean.getActiveConnections
    List(dsPgStatus, dsFireBaseStatus, dsNextCloudStatus).mkString(",")
  }
}
