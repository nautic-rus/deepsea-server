package deepsea.database

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.DatabaseManagerStarted
import deepsea.database.DatabaseManager.{GetConnectionFromPool, GetFireBaseConnectionFromPool, GetMongoConnectionFromPool, GetOracleConnectionFromPool, codecRegistry}
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.sql.{Connection, DriverManager}
import java.util.concurrent.TimeUnit
import scala.concurrent.Await

object DatabaseManager extends MongoCodecs {
  implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)
  case class GetConnectionFromPool()
  case class ReleaseConnectionFromPool(connection: Connection)
  case class GetMongoConnectionFromPool()
  case class GetOracleConnectionFromPool()
  case class GetFireBaseConnectionFromPool()
  case class OracleConnection(project: String, ds: HikariDataSource)

  def GetConnection(): Option[Connection] ={
    try{
      Await.result(ActorManager.dataBase ? GetConnectionFromPool(), timeout.duration) match {
        case response: Connection => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
  def GetMongoConnection(): Option[MongoDatabase] = {
    try{
      Await.result(ActorManager.dataBase ? GetMongoConnectionFromPool(), timeout.duration) match {
        case response: MongoDatabase => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
  def GetOracleConnection(): Option[Connection] ={
    try{
      Await.result(ActorManager.dataBase ? GetOracleConnectionFromPool(), timeout.duration) match {
        case response: Connection => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
  def GetFireBaseConnection(): Option[Connection] ={
    try{
      Await.result(ActorManager.dataBase ? GetFireBaseConnectionFromPool(), timeout.duration) match {
        case response: Connection => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
  def ReleaseConnection(connection: Connection){
    ActorManager.dataBase ! ReleaseConnectionFromPool(connection)
  }
}
class DatabaseManager extends Actor{

  private val config = new HikariConfig()
  private var ds: HikariDataSource = _

  private val configOracle = new HikariConfig()
  private var dsOracle: HikariDataSource = _

  private val configFireBase = new HikariConfig()
  private var dsFireBase: HikariDataSource = _

  private var mongoClient: MongoClient = _

  override def preStart(): Unit = {
    config.setDriverClassName("org.postgresql.Driver")
    config.setJdbcUrl("jdbc:postgresql://192.168.1.26/deepsea")
    config.setUsername("deepsea")
    config.setPassword("Ship1234")
    ds = new HikariDataSource(config)

    configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
    configOracle.setUsername("CN002")
    configOracle.setPassword("Whatab0utus")
    configOracle.setMaximumPoolSize(3)
    dsOracle = new HikariDataSource(configOracle)

    configFireBase.setDriverClassName("org.firebirdsql.jdbc.FBDriver")
    configFireBase.setJdbcUrl("jdbc:firebirdsql://192.168.1.21:3053/C:/Program Files (x86)/TimeControl/BASE/OKO.FDB?charSet=UTF8")
    configFireBase.setUsername("MEGA")
    configFireBase.setPassword("STMEGA21")
    dsFireBase = new HikariDataSource(configFireBase)

    mongoClient = MongoClient("mongodb://192.168.1.26")

    ActorManager.startup ! DatabaseManagerStarted()
  }
  override def receive: Receive = {
    case GetConnectionFromPool() => sender() ! ds.getConnection
    case GetOracleConnectionFromPool() => sender() ! dsOracle.getConnection
    case GetFireBaseConnectionFromPool() => sender() ! dsFireBase.getConnection
    case GetMongoConnectionFromPool() => sender() ! mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    case _ => None
  }
}
