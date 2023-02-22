package deepsea.auth

import akka.actor.Actor
import deepsea.auth.AuthManager.{DeleteRole, DeleteUser, EditRole, EditUser, GetPages, GetRightDetails, GetRights, GetRoleDetails, GetRoles, GetUser, GetUserDetails, GetUsers, Login, Page, RightUser, Role, ShareRights, StartRole, StartUser, UpdateEmail, UpdateRocketLogin, User}
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
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object AuthManager extends MongoCodecs {
  case class Login(token: Option[String], login: String = "", password: String = "")

  case class GetUsers()

  case class GetUserDetails(id: String)

  case class GetUser(login: String)

  case class StartUser(userJson: String)

  case class DeleteUser(id: String)

  case class EditUser(userJson: String, id: String)

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
                   var projects: List[String]
                 )

  case class GetRoles()

  case class GetRoleDetails(name: String)

  case class StartRole(roleJson: String)

  case class DeleteRole(name: String)

  case class EditRole(roleJson: String, name: String)

  case class Role(
                   name: String,
                   description: String,
                   visible_pages: List[String]
                 )

  case class GetPages()

  case class Page(
                   id: Int,
                   name: String
                 )

  case class GetRights()

  case class GetRightDetails(id: String)

  case class RightUser(
                        userId: Int,
                        role: String
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
          val result = startRole (role)
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
    case GetPages() => sender() ! getPages().asJson
    case GetRights() => sender() ! getRights().asJson
    case GetRightDetails(id) => sender() ! getRightDetails(id).asJson
    case GetUser(login) =>
      getUser(login) match {
        case Some(user) => sender() ! user
        case _ => sender() ! Option.empty[User]
      }
    case StartUser(userJson) =>
      circe.jawn.decode[User](userJson) match {
        case Right(user) =>
          val result = startUser(user)
          sender() ! result.asJson
        case _ => sender() ! "error".asJson
      }
    case DeleteUser(id) => sender() ! deleteUser(id).asJson
    case EditUser(userJson, id) =>
      circe.jawn.decode[User](userJson) match {
        case Right(user) =>
          val result = editUser(user, id)
          sender() ! result.asJson
        case _ => sender() ! "error".asJson
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
          val id = rs.getInt("id");
          res += User(
            id,
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
            getRightDetails(id.toString).toList,
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
            getRightDetails(id.toString).toList,
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

  def startUser(user: User): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"insert into users (id, login, password, name, surname, birthday, email, phone, tcid, avatar, profession, visibility, gender, avatar_full, department, rocket_login, visible_projects, \"group\", projects) " +
          s"values (default, '${user.login}', '${user.password}', '${user.name}', '${user.surname}', '${user.birthday}', '${user.email}', '${user.phone}', ${user.tcid}, '${user.avatar}', '${user.profession}', '${user.visibility}', '${user.gender}', '${user.avatar_full}', '${user.department}', '${user.rocket_login}', '${user.visible_projects.mkString(",")}','${user.groups.mkString(",")}', '${user.projects.mkString(",")}')" +
          s" returning id"
        val rs = s.executeQuery(query);
        while (rs.next()) {
          user.permissions.foreach(role => {
            val query = s"insert into user_rights (user_id, rights) values ('${rs.getInt("id")}', '$role')";
            s.execute(query);
          })
        }
        s.close();
        c.close();
        "success"
    }
  }

  def deleteUser(id: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"update users set removed = 1 where id = $id";
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error"
    }
  }

  def editUser(user: User, id: String) = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"update users set id = '${user.id}', login = '${user.login}', password = '${user.password}', name = '${user.name}', surname = '${user.surname}', birthday = '${user.birthday}', email = '${user.email}', phone = '${user.phone}', tcid = '${user.tcid}', avatar = '${user.avatar}', profession = '${user.profession}', visibility = '${user.visibility}', gender = '${user.gender}', department = '${user.department}', rocket_login = '${user.rocket_login}' where id = '$id'"
        s.execute(query);
        val queryR = s"delete from user_rights where user_id = '$id'";
        s.execute(queryR);
        user.permissions.foreach(role => {
          val query = s"insert into user_rights (user_id, rights) values ('$id', '$role')";
          s.execute(query);
        })
        s.close();
        c.close();
        "success"
      case _ => "error";
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
            rs.getString("description"),
            rs.getString("visible_pages").split(",").toList,
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
          role = Option(Role(
            rs.getString("name"),
            rs.getString("description"),
            rs.getString("visible_pages").split(",").toList
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
        var line = "";
        val query = s"insert into roles (name, description, visible_pages) values ('${role.name}', '${role.description}', '${role.visible_pages.mkString(",")}')";
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
        val query = s"update roles set description = '${role.description}', visible_pages = '${role.visible_pages.mkString(",")}' where name = '$name'";
        s.execute(query);
        s.close();
        c.close();
        "success"
      case _ => "error";
    }
  }

  def getPages(): ListBuffer[Page] = {
    val res = ListBuffer.empty[Page];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from pages");
        while (rs.next()) {
          res += Page(
            rs.getInt("id"),
            rs.getString("name")
          )
        }
        rs.close();
        s.close();
        c.close();
        res
      case _ => ListBuffer.empty[Page]
    }
  }

  def getRights(): ListBuffer[RightUser] = {
    val res = ListBuffer.empty[RightUser];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from user_rights");
        while (rs.next()) {
          res += RightUser(
            rs.getInt("user_id"),
            rs.getString("rights")
          )
        }
        rs.close();
        s.close();
        c.close();
        res
      case _ => ListBuffer.empty[RightUser]
    }
  }

  def getRightDetails(id: String): ListBuffer[String] = {
    val res = ListBuffer.empty[String];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from user_rights where user_id = '$id'");
        while (rs.next()) {
          res += (
            rs.getString("rights")
            )
        }
        rs.close();
        s.close();
        c.close();
        res
      case _ => ListBuffer.empty[String]
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
