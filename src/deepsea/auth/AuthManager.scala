package deepsea.auth

import akka.actor.Actor
import deepsea.auth.AuthManager.{GetUser, GetUsers, Login, ShareRights, User, writesUser}
import deepsea.database.DatabaseManager.GetConnection
import play.api.libs.json.{Json, OWrites}

import java.sql.Date
import java.util.UUID
import scala.collection.mutable.ListBuffer

object AuthManager{
  case class Login(token: Option[String], login: String = "", password: String = "")
  case class GetUsers()
  case class GetUser(login: String)
  case class ShareRights(user: String, with_user: String)
  case class User(
                   id: Int, login: String, password: String, name: String, surname: String, profession: String, department: String, birthday: Date, email: String,
                   phone: String, tcid: Int, avatar: String, avatar_full: String, rocket_login: String, var gender: String, var visibility: String,
                   var visible_projects: List[String], var visible_pages: List[String], var shared_access: List[String], var token: String = "", var groups: ListBuffer[String] = ListBuffer.empty[String],
                   var permissions: ListBuffer[String] = ListBuffer.empty[String])
  implicit val writesUser: OWrites[User] = Json.writes[User]
}
class AuthManager extends Actor{
  override def receive: Receive = {
    case Login(token, login, password) =>
      token match {
        case Some(token) =>
          getUserByToken(token) match {
            case Some(userLogin) =>
              getUser(userLogin) match {
                case Some(user) =>
                  user.token = token
                  sender() ! Json.toJson(user)
                case _ => None
              }
            case _ => sender() ! Json.toJson("wrong-token")
          }
        case _ =>
          getUserByLoginPassword(login, password) match {
            case Some(userLogin) =>
              addUserToken(userLogin) match {
                case Some(token) =>
                  getUser(userLogin) match {
                    case Some(user) =>
                      user.token = token
                      sender() ! Json.toJson(user)
                    case _ => None
                  }
                case _ => None
              }
            case _ => sender() ! Json.toJson("wrong-password")
          }
      }
    case GetUsers() => sender() ! Json.toJson(getUsers)
    case GetUser(login) =>
      getUser(login) match {
        case Some(user) => sender() ! user
        case _ => sender() ! Option.empty[User]
      }
    case ShareRights(user, with_user) =>
      shareWith(user, with_user)
      sender() ! Json.toJson("success")
    case _ => None
  }
  def addUserToken(user: String): Option[String] ={
    val token = UUID.randomUUID().toString
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"insert into sessions values ('$user', '$token')")
        s.close()
        c.close()
        Option(token)
      case _ => Option.empty[String]
    }
  }
  def getUserByToken(token: String): Option[String] ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select s.user from sessions s where token = '$token'")
        var res = Option.empty[String]
        while (rs.next()){
          res = Option(rs.getString("user"))
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => Option.empty[String]
    }
  }
  def getUserByLoginPassword(login: String, password: String): Option[String] ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select login from users where login = '$login' and password = '$password'")
        var res = Option.empty[String]
        while (rs.next()){
          res = Option(rs.getString("login"))
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => Option.empty[String]
    }
  }
  def getUser(login: String): Option[User] ={
    GetConnection() match {
      case Some(c) =>
        var s = c.createStatement()
        var rs = s.executeQuery(s"select * from users where login = '$login'")
        var res = Option.empty[User]
        while (rs.next()){
          res = Option(User(
            rs.getInt("id"),
            rs.getString("login"),
            rs.getString("password"),
            rs.getString("name"),
            rs.getString("surname"),
            rs.getString("profession"),
            rs.getString("department"),
            rs.getDate("birthday"),
            rs.getString("email"),
            rs.getString("phone"),
            rs.getInt("tcid"),
            rs.getString("avatar"),
            rs.getString("avatar_full"),
            rs.getString("rocket_login"),
            rs.getString("gender"),
            rs.getString("visibility"),
            rs.getString("visible_projects").split(",").toList,
            rs.getString("visible_pages").split(",").toList,
            rs.getString("shared_access").split(",").toList
          ))
        }
        s.close()
        if (res.nonEmpty){
          s = c.createStatement()
          rs = s.executeQuery(s"select type_name from issue_types where id in (select group_id from user_membership where user_id = ${res.get.id})")
          while (rs.next()){
            res.get.groups += rs.getString("type_name")
          }
          s.close()
          s = c.createStatement()
          rs = s.executeQuery(s"select rights from user_rights where user_id = ${res.get.id}")
          while (rs.next()){
            res.get.permissions += rs.getString("rights")
          }
          rs.close()
          s.close()
        }
        c.close()
        res
      case _ => Option.empty[User]
    }
  }
  def getUsers: ListBuffer[User] ={
    val res = ListBuffer.empty[User]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users")
        while (rs.next()){
          res += User(
            rs.getInt("id"),
            rs.getString("login"),
            rs.getString("password"),
            rs.getString("name"),
            rs.getString("surname"),
            rs.getString("profession"),
            rs.getString("department"),
            rs.getDate("birthday"),
            rs.getString("email"),
            rs.getString("phone"),
            rs.getInt("tcid"),
            rs.getString("avatar"),
            rs.getString("avatar_full"),
            rs.getString("rocket_login"),
            rs.getString("gender"),
            rs.getString("visibility"),
            rs.getString("visible_projects").split(",").toList,
            rs.getString("visible_pages").split(",").toList,
            rs.getString("shared_access").split(",").toList
          )
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[User]
    }
  }
  def shareWith(user: String, with_user: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update users set shared_access = '$user' where login = '$with_user'")
        s.close()
        c.close()
      case _ => None
    }
  }
}
