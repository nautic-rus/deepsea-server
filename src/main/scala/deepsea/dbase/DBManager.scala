package deepsea.dbase

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.dbase.DatabaseManager.OracleConnection
import org.mongodb.scala.{MongoClient, MongoDatabase}
import slick.jdbc.JdbcBackend
import slick.jdbc.JdbcBackend.Database

import java.sql.{Connection, ResultSet}
import scala.collection.mutable.ListBuffer

object DBManager extends MongoCodecs {

  case class RsIterator(rs: ResultSet) extends Iterator[ResultSet] {
    def hasNext: Boolean = try {
      rs.next()
    } catch {
      case e: Exception =>
        println(s"Error in hasNext: ${e.getMessage}")
        false
    }

    def next(): ResultSet = try {
      rs
    } catch {
      case e: Exception =>
        println(s"Error in next: ${e.getMessage}")
        throw e
    }
  }

  private val configOracle = new HikariConfig()
  private val oracleConnections = ListBuffer.empty[OracleConnection]
  private val mongoClient: MongoClient = try {
    MongoClient("mongodb://192.168.1.36")
  } catch {
    case e: Exception =>
      println(s"Error initializing MongoClient: ${e.getMessage}")
      null
  }

  List("N002", "N004", "N007", "SC01", "LV01").foreach(project => {
    try {
      configOracle.setDriverClassName("oracle.jdbc.OracleDriver")
      configOracle.setJdbcUrl("jdbc:oracle:thin:@192.168.1.12:1521:ORA3DB")
      configOracle.setUsername("C" + project)
      configOracle.setPassword("Whatab0utus")
      configOracle.setMaximumPoolSize(2)
      oracleConnections += OracleConnection(project, new HikariDataSource(configOracle))
    } catch {
      case e: Exception =>
        println(s"Error initializing Oracle connection for project $project: ${e.getMessage}")
    }
  })

  private val configPG = new HikariConfig()
  var dsPG: HikariDataSource = try {
    configPG.setDriverClassName("org.postgresql.Driver")
    configPG.setJdbcUrl("jdbc:postgresql://192.168.1.26/deepsea")
    configPG.setUsername("deepsea")
    configPG.setPassword("Ship1234")
    configPG.setMaximumPoolSize(10)
    new HikariDataSource(configPG)
  } catch {
    case e: Exception =>
      println(s"Error initializing PostgreSQL datasource: ${e.getMessage}")
      null
  }

  val PostgresSQL: JdbcBackend.Database = try {
    Database.forDataSource(dsPG, Option(5))
  } catch {
    case e: Exception =>
      println(s"Error initializing PostgreSQL database: ${e.getMessage}")
      null
  }

  private val configPGFest = new HikariConfig()
  val dsPGFest: HikariDataSource = try {
    configPGFest.setDriverClassName("org.postgresql.Driver")
    configPGFest.setJdbcUrl("jdbc:postgresql://192.168.1.26/nautic_fest")
    configPGFest.setUsername("deepsea")
    configPGFest.setPassword("Ship1234")
    configPGFest.setMaximumPoolSize(5)
    new HikariDataSource(configPGFest)
  } catch {
    case e: Exception =>
      println(s"Error configPGFest: ${e.getMessage}")
      null
  }

//  private val configNextCloud = new HikariConfig()
//  var dsNextCloud: HikariDataSource = try {
//    configNextCloud.setDriverClassName("org.postgresql.Driver")
//    configNextCloud.setJdbcUrl("jdbc:postgresql://cloud.nautic-rus.com:5570/nextcloud_database")
//    configNextCloud.setUsername("oc_nextcloud")
//    configNextCloud.setPassword("cab5a0e8b5db655ec2d8ce46efd22cea3f5b137ee450deb3")
//    configNextCloud.setMaximumPoolSize(5)
//    new HikariDataSource(configNextCloud)
//  } catch {
//    case e: Exception =>
//      println(s"Error configNextCloud: ${e.getMessage}")
//      null
//  }

  val configFireBase = new HikariConfig()
  val dsFireBase: HikariDataSource = try {
    configFireBase.setDriverClassName("org.firebirdsql.jdbc.FBDriver")
    configFireBase.setJdbcUrl("jdbc:firebirdsql://192.168.1.21:3053/C:/Program Files (x86)/TimeControl/BASE/OKO.FDB?charSet=UTF8")
    configFireBase.setUsername("MEGA")
    configFireBase.setPassword("STMEGA21")
    new HikariDataSource(configFireBase)
  } catch {
    case e: Exception =>
      println(s"Error initializing FireBase datasource: ${e.getMessage}")
      null
  }

  def GetOracleConnection(project: String): Option[Connection] = {
    try {
      oracleConnections.find(_.project == project) match {
        case Some(connection) => Option(connection.ds.getConnection)
        case _ => Option.empty
      }
    } catch {
      case e: Exception =>
        println(s"Error getting Oracle connection for project $project: ${e.getMessage}")
        Option.empty
    }
  }

  def GetPGConnection(): Option[Connection] = {
    try {
      Option(dsPG.getConnection)
    } catch {
      case e: Throwable =>
        println(s"Error getting PG connection: ${e.getMessage}")
        dsPG = new HikariDataSource(configPG)
        Option.empty
    }
  }

  def GetPGFestConnection(): Option[Connection] = {
    try {
      Option(dsPGFest.getConnection)
    } catch {
      case e: Exception =>
        println(s"Error getting PGFest connection: ${e.getMessage}")
        Option.empty
    }
  }

  def GetMongoConnection(): Option[MongoDatabase] = {
    try {
      Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
    } catch {
      case e: Exception =>
        println(s"Error getting Mongo connection: ${e.getMessage}")
        Option.empty
    }
  }

  def GetMongoCacheConnection(): Option[MongoDatabase] = {
    try {
      Option(mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry))
    } catch {
      case e: Exception =>
        println(s"Error getting MongoCache connection: ${e.getMessage}")
        Option.empty
    }
  }

  def GetMongoFilesConnection(): Option[MongoDatabase] = {
    try {
      Option(mongoClient.getDatabase("files").withCodecRegistry(codecRegistry))
    } catch {
      case e: Exception =>
        println(s"Error getting MongoFiles connection: ${e.getMessage}")
        Option.empty
    }
  }

//  def GetNextCloudConnection(): Option[Connection] = {
//    try {
//      Option(dsNextCloud.getConnection)
//    } catch {
//      case e: Exception =>
//        println(s"Error getting NextCloud connection: ${e.getMessage}")
//        Option.empty
//    }
//  }

  def GetFireBaseConnection(): Option[Connection] = {
    try {
      Option(dsFireBase.getConnection)
    } catch {
      case e: Exception =>
        println(s"Error getting FireBase connection: ${e.getMessage}")
        Option.empty
    }
  }

  def check(): String = {
    try {
      val dsPgStatus = "dsPg " + dsPG.getHikariPoolMXBean.getActiveConnections
      val dsFireBaseStatus = "dsFireBase " + dsFireBase.getHikariPoolMXBean.getActiveConnections
//      val dsNextCloudStatus = "dsNextCloud " + dsNextCloud.getHikariPoolMXBean.getActiveConnections
      List(dsPgStatus, dsFireBaseStatus).mkString(",")
    } catch {
      case e: Exception =>
        println(s"Error in check method: ${e.getMessage}")
        "Error checking connections"
    }
  }
}

//package deepsea.dbase
//
//import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
//import deepsea.dbase.DatabaseManager.OracleConnection
//import org.mongodb.scala.{MongoClient, MongoDatabase}
//import slick.jdbc.JdbcBackend
//import slick.jdbc.JdbcBackend.Database
//
//import java.sql.{Connection, ResultSet}
//import scala.collection.mutable.ListBuffer
//
//object DBManager extends MongoCodecs {
//
//  case class RsIterator(rs: ResultSet) extends Iterator[ResultSet] {
//    def hasNext: Boolean = rs.next()
//    def next(): ResultSet = rs
//  }
//
//  private val configOracle = new HikariConfig()
//  private val oracleConnections = ListBuffer.empty[OracleConnection]
//  private val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.36")
//
//  //  JSch.setConfig("StrictHostKeyChecking", "no")
////  val nextcloudSSH = new JSch()
////  val nextcloudSSHSession: Session = nextcloudSSH.getSession("root", "89.108.125.21")
////  nextcloudSSHSession.setPassword("Whatab0utus")
////  nextcloudSSHSession.connect()
////  nextcloudSSHSession.setPortForwardingL("localhost", 1155, "172.18.0.5", 5432)
//
//
//  List("N002", "N004", "N007", "SC01", "LV01").foreach(project => {
//    try{
//      configOracle.setDriverClassName("oracle.jdbc.OracleDriver")
//      configOracle.setJdbcUrl("jdbc:oracle:thin:@192.168.1.12:1521:ORA3DB")
//      configOracle.setUsername("C" + project)
//      configOracle.setPassword("Whatab0utus")
//      configOracle.setMaximumPoolSize(2)
//      oracleConnections += OracleConnection(project, new HikariDataSource(configOracle))
//    }
//    catch {
//      case e: Exception =>
//    }
//  })
//
//  private val configPG = new HikariConfig()
//  try {
//
//  }
//  catch {
//    case e: Exception => println(e)
//  }
//  configPG.setDriverClassName("org.postgresql.Driver")
//  configPG.setJdbcUrl("jdbc:postgresql://192.168.1.26/deepsea")
//  configPG.setUsername("deepsea")
//  configPG.setPassword("Ship1234")
//  configPG.setMaximumPoolSize(10)
//  var dsPG = new HikariDataSource(configPG)
//
//  val PostgresSQL: JdbcBackend.Database = Database.forDataSource(dsPG, Option(5))
//
//  private val configPGFest = new HikariConfig()
//  configPGFest.setDriverClassName("org.postgresql.Driver")
//  configPGFest.setJdbcUrl("jdbc:postgresql://192.168.1.26/nautic_fest")
//  configPGFest.setUsername("deepsea")
//  configPGFest.setPassword("Ship1234")
//  configPGFest.setMaximumPoolSize(5)
//  val dsPGFest = new HikariDataSource(configPGFest)
//
////  private val configNextCloud = new HikariConfig()
////  configNextCloud.setDriverClassName("org.postgresql.Driver")
////  configNextCloud.setJdbcUrl("jdbc:postgresql://localhost:1155/nextcloud_database")
////  configNextCloud.setUsername("oc_nextcloud")
////  configNextCloud.setPassword("cab5a0e8b5db655ec2d8ce46efd22cea3f5b137ee450deb3")
////  configNextCloud.setMaximumPoolSize(25)
////  var dsNextCloud = new HikariDataSource(configNextCloud)
//
//  private val configNextCloud = new HikariConfig()
//  configNextCloud.setDriverClassName("org.postgresql.Driver")
//  configNextCloud.setJdbcUrl("jdbc:postgresql://cloud.nautic-rus.com:5570/nextcloud_database")
//  configNextCloud.setUsername("oc_nextcloud")
//  configNextCloud.setPassword("cab5a0e8b5db655ec2d8ce46efd22cea3f5b137ee450deb3")
//  configNextCloud.setMaximumPoolSize(5)
//  var dsNextCloud = new HikariDataSource(configNextCloud)
//
//  val configFireBase = new HikariConfig()
//  configFireBase.setDriverClassName("org.firebirdsql.jdbc.FBDriver")
//  configFireBase.setJdbcUrl("jdbc:firebirdsql://192.168.1.21:3053/C:/Program Files (x86)/TimeControl/BASE/OKO.FDB?charSet=UTF8")
//  configFireBase.setUsername("MEGA")
//  configFireBase.setPassword("STMEGA21")
//  val dsFireBase = new HikariDataSource(configFireBase)
//
//
//  def GetOracleConnection(project: String): Option[Connection] ={
//    oracleConnections.find(_.project == project) match {
//      case Some(connection) => Option(connection.ds.getConnection)
//      case _ => Option.empty
//    }
//  }
//  def GetPGConnection(): Option[Connection] = {
//    try {
//      Option(dsPG.getConnection)
//    }
//    catch {
//      case e: Throwable =>
//        println(e.getMessage)
//        dsPG = new HikariDataSource(configPG)
//        Option.empty
//    }
//  }
//  def GetPGFestConnection(): Option[Connection] = {
//    Option(dsPGFest.getConnection)
//  }
//  def GetMongoConnection(): Option[MongoDatabase] = {
//    Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
//  }
//  def GetMongoCacheConnection(): Option[MongoDatabase] = {
//    Option(mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry))
//  }
//  def GetMongoFilesConnection(): Option[MongoDatabase] = {
//    Option(mongoClient.getDatabase("files").withCodecRegistry(codecRegistry))
//  }
//  def GetNextCloudConnection(): Option[Connection] = {
//    Option(dsNextCloud.getConnection)
//  }
//  def GetFireBaseConnection(): Option[Connection] = {
//    Option(dsFireBase.getConnection)
//  }
//  def check(): String ={
//    val dsPgStatus = "dsPg " + dsPG.getHikariPoolMXBean.getActiveConnections
//    val dsFireBaseStatus = "dsFireBase " + dsFireBase.getHikariPoolMXBean.getActiveConnections
//    val dsNextCloudStatus = "dsNextCloud " + dsNextCloud.getHikariPoolMXBean.getActiveConnections
//    List(dsPgStatus, dsFireBaseStatus, dsNextCloudStatus).mkString(",")
//  }
//}
