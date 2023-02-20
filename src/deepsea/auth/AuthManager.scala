package deepsea.auth

import akka.actor.Actor
import deepsea.auth.AuthManager.{DeleteRole, EditRole, GetRoleDetails, GetRoles, GetUser, GetUserDetails, GetUsers, Login, Role, ShareRights, StartRole, UpdateEmail, UpdateRocketLogin, User}
import deepsea.database.{DBManager, MongoCodecs}
import io.circe
import io.circe.jawn
import io.circe.syntax.EncoderOps
import play.api.libs.json.{Json, OWrites}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.mongodb.scala.result

import java.sql.Date
import java.util.UUID
import scala.collection.mutable.ListBuffer

object AuthManager extends MongoCodecs {
  case class Login(token: Option[String], login: String = "", password: String = "")

  case class GetUsers()

  case class GetUserDetails(id: String)

  case class GetUser(login: String)

  case class ShareRights(user: String, with_user: String)

  case class UpdateEmail(user: String, email: String)

  case class UpdateRocketLogin(user: String, rocketLogin: String)

  case class User(
                   id: Int,
                   login: String,
                   password: String,
                   name: String,
                   surname: String,
                   profession: String,
                   department: String,
                   birthday: String,
                   email: String,
                   phone: String,
                   tcid: Int,
                   avatar: String,
                   avatar_full: String,
                   rocket_login: String,
                   var gender: String,
                   var visibility: String,
                   var visible_projects: List[String],
                   var visible_pages: List[String],
                   var shared_access: List[String],
                   var groups: List[String] = List.empty[String],
                   var permissions: List[String] = List.empty[String],
                   var token: String = "",
                   var projects: List[String],
                 )

  case class GetRoles()

  case class GetRoleDetails(name: String)

  case class StartRole(roleJson: String)

  case class DeleteRole(name: String)

  case class EditRole(roleJson: String, name: String)

  case class Role(
                   name: String,
                   description: String
                 )
}

class AuthManager extends Actor with AuthManagerHelper with MongoCodecs {
  override def receive: Receive = {
    case Login(token, login, password) =>
      token match {
        case Some(token) =>
          getUserByToken(token) match {
            case Some(userLogin) =>
              getUser(userLogin) match {
                case Some(user) =>
                  user.token = token
                  sender() ! user.asJson.noSpaces
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
                      sender() ! user.asJson.noSpaces
                    case _ => None
                  }
                case _ => None
              }
            case _ => sender() ! Json.toJson("wrong-password")
          }
      }
    case GetUsers() => sender() ! getUsers.asJson.noSpaces
    case GetUserDetails(id) => sender() ! getUserDetails(id).asJson.noSpaces
    case GetRoles() => sender() ! getRoles.asJson
    case GetRoleDetails(name) => sender() ! getRoleDetails(name).asJson
    case StartRole(roleJson) =>
      circe.jawn.decode[Role](roleJson) match {
        case Right(role) =>
          val result = startRole(role)
          sender() ! result.asJson
        case _ => sender() ! "error".asJson
      }
    case DeleteRole(name) => sender() ! deleteRole(name).asJson
    case EditRole(rolJson, name) =>
      circe.jawn.decode[Role](rolJson) match {
        case Right(role) =>
          val result = editRole(role, name)
          sender() ! result.asJson
        case _ => sender() ! "error".asJson
      }


    case GetUser(login) =>
      getUser(login) match {
        case Some(user) => sender() ! user
        case _ => sender() ! Option.empty[User]
      }
    case ShareRights(user, with_user) =>
      shareWith(user, with_user)
      sender() ! Json.toJson("success")
    case UpdateEmail(user, email) =>
      sender() ! updateEmail(user, email)
      sender() ! Json.toJson("success")
    case UpdateRocketLogin(user, rocketLogin) =>
      sender() ! updateRocketLogin(user, rocketLogin)
      sender() ! Json.toJson("success")
    case _ => None
  }

  def addUserToken(user: String): Option[String] = {
    val token = UUID.randomUUID().toString
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"" +
          s"insert into sessions values ('$user', '$token')")
        s.close()
        c.close()
        Option(token)
      case _ => Option.empty[String]
    }
  }

  def getUserByToken(token: String): Option[String] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select s.user from sessions s where token = '$token'")
        var res = Option.empty[String]
        while (rs.next()) {
          res = Option(rs.getString("user"))
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => Option.empty[String]
    }
  }

  def getUserByLoginPassword(login: String, password: String): Option[String] = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select login from users where login = '$login' and password = '$password' and removed = 0")
        var res = Option.empty[String]
        while (rs.next()) {
          res = Option(rs.getString("login"))
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => Option.empty[String]
    }
  }

  def getUsers: ListBuffer[User] = {
    val res = ListBuffer.empty[User]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users where removed = 0")
        while (rs.next()) {
          res += User(
            rs.getInt("id"),
            rs.getString("login"),
            rs.getString("password"),
            rs.getString("name"),
            rs.getString("surname"),
            rs.getString("profession"),
            rs.getString("department"),
            rs.getDate("birthday").toString,
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
            rs.getString("shared_access").split(",").toList,
            rs.getString("group").split(",").toList,
            List.empty[String],
            "",
            rs.getString("projects").split(",").toList
          )
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[User]
    }
  }

  def getUserDetails(id: String): Option[User] = {
    var user: Option[User] = Option.empty[User]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users where id = '$id'")
        while (rs.next()) {
          user = Option(new User(rs.getInt("id"),
            rs.getString("login"),
            rs.getString("password"),
            rs.getString("name"),
            rs.getString("surname"),
            rs.getString("profession"),
            rs.getString("department"),
            rs.getDate("birthday").toString,
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
            rs.getString("shared_access").split(",").toList,
            rs.getString("group").split(",").toList,
            List.empty[String],
            "",
            rs.getString("projects").split(",").toList))
        }
        rs.close()
        s.close()
        c.close()
        user
      case _ => Option.empty[User]
    }
  }

  def getRoles: ListBuffer[Role] = {
    val res = ListBuffer.empty[Role]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from roles")
        while (rs.next()) {
          res += Role(
            rs.getString("name"),
            rs.getString("description")
          )
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[Role]
    }
  }
  def getRoleDetails(name: String): Option[Role] = {
    var role: Option[Role] = Option.empty[Role]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from roles where name = '$name'")
        while (rs.next()) {
          role = Option(new Role(
            rs.getString("name"),
            rs.getString("description")
          ))
        }
        rs.close()
        s.close()
        c.close()
        role
      case _ => Option.empty[Role]
    }
  }
  def deleteRole(name: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"delete from roles where name = '$name'";
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }
  def startRole(role: Role): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"insert into roles (name, description) values ('${role.name}', '${role.description}')";
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }
  def editRole(role: Role, name: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"update roles set name = '${role.name}', description = '${role.description}' where name = '$name'";
        s.execute(query);
        s.close();
        c.close();
        "success"
      case _ => "error";
    }
  }

  def shareWith(user: String, with_user: String): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update users set shared_access = '$user' where login = '$with_user'")
        s.close()
        c.close()
      case _ => None
    }
  }
}
