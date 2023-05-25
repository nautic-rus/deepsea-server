package deepsea.auth

import akka.actor.Actor
import com.sun.mail.imap.Rights
import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.{AdminRight, DeleteAdminRight, DeleteRole, DeleteUser, Department, EditAdminRight, EditRole, EditUser, EditUsersProject, GetAdminRightDetails, GetAdminRights, GetDepartmentDetails, GetDepartments, GetPages, GetRightDetails, GetRights, GetRoleDetails, GetRoleRights, GetRoles, GetUser, GetUserDetails, GetUserVisibleProjects, GetUsers, GetUsersProject, JoinUsersProjects, Login, Page, RightUser, Role, SaveRoleForAll, SendLogPass, ShareRights, StartRight, StartRole, StartUser, UpdateEmail, UpdateRocketLogin, User}
import deepsea.database.{DBManager, MongoCodecs}
import deepsea.issues.IssueManager.IssueProject
import deepsea.issues.{IssueManager, IssueManagerHelper}
import deepsea.mail.MailManager.Mail
import deepsea.rocket.RocketChatManager.SendNotification
import io.circe
import io.circe.jawn
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.mongodb.scala.result

import java.sql.Date
import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object AuthManager extends MongoCodecs {
  case class Login(token: Option[String], login: String = "", password: String = "")

  case class GetUsers()

  case class GetUserDetails(id: String)

  case class GetUser(login: String)

  case class StartUser(userJson: String)

  case class DeleteUser(id: String)

  case class EditUser(userJson: String, id: String)

  case class EditUsersProject(idUsers: String, idProject: String)

  case class JoinUsersProjects()

  case class GetUsersProject(id: String)

  case class GetUserVisibleProjects(id: String)

  case class UserProject(user_id: String, project_name: String)

  case class SendLogPass(id: String)

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
                   var id_department: Int
                 )

  case class GetRoles()

  case class GetRoleDetails(name: String)

  case class GetRoleRights(name: String)

  case class StartRole(roleJson: String)

  case class SaveRoleForAll(name: String)

  case class DeleteRole(name: String)

  case class EditRole(roleJson: String, name: String)

  case class Role(
                   name: String,
                   description: String,
                   rights: List[String],
                   pages: List[String],
                 )

  case class RolePage(
                       role: String,
                       page: String
                     )

  case class GetAdminRights()

  case class GetAdminRightDetails(name: String)

  case class DeleteAdminRight(name: String)

  case class EditAdminRight(rightJson: String, name: String)

  case class AdminRight(
                         name: String
                       )

  case class StartRight(rightJson: String)

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

  case class Department(
                         id: Int,
                         name: String,
                         manager: String,
                         visible_documents: Int,
                         visible_man_hours: Int
                       )

  case class GetDepartments()

  case class GetDepartmentDetails(id: String)
}

class AuthManager extends Actor with AuthManagerHelper with IssueManagerHelper with MongoCodecs {
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
            case _ => sender() ! ("wrong-token").asJson.noSpaces
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
            case _ => sender() !("wrong-password").asJson.noSpaces
          }
      }
    case GetUsers() =>
      sender() ! getUsers.asJson.noSpaces
    case GetUserDetails(id) => sender() ! getUserDetails(id).asJson.noSpaces
    case GetRoles() => sender() ! getRoles.asJson.noSpaces
    case GetRoleDetails(name) => sender() ! getRoleDetails(name).asJson.noSpaces
    case GetRoleRights(name) => sender() ! getRoleRights(name).asJson.noSpaces
    case StartRole(roleJson) =>
      circe.jawn.decode[Role](roleJson) match {
        case Right(role) =>
          val result = startRole(role)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case SaveRoleForAll(name) => sender() ! saveRoleForAll(name)
    case DeleteRole(name) => sender() ! deleteRole(name).asJson.noSpaces
    case EditRole(rolJson, name) =>
      circe.jawn.decode[Role](rolJson) match {
        case Right(role) =>
          val result = editRole(role, name)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case GetAdminRights() => sender() ! getAdminRights.asJson.noSpaces
    case GetAdminRightDetails(name) => sender() ! getAdminRightsDetails(name).asJson.noSpaces
    case DeleteAdminRight(name) => sender() ! deleteAdminRight(name).asJson.noSpaces
    case EditAdminRight(rightJson, name) =>
      circe.jawn.decode[AdminRight](rightJson) match {
        case Right(right) =>
          val result = editAdminRight(right, name)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case StartRight(rightJson) =>
      circe.jawn.decode[AdminRight](rightJson) match {
        case Right(right) =>
          val result = startRight(right)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case GetPages() => sender() ! getPages.asJson.noSpaces
    case GetRights() => sender() ! getRights.asJson.noSpaces
    case GetRightDetails(id) => sender() ! getRightDetails(id.toIntOption.getOrElse(0)).asJson.noSpaces
    case GetDepartments() => sender() ! getDepartments.asJson
    case GetDepartmentDetails(id) => sender() ! getDepartmentDetail(id).asJson.noSpaces
    case GetUser(login) =>
      getUser(login) match {
        case Some(user) => sender() ! user
        case _ => sender() ! Option.empty[User]
      }
    case StartUser(userJson) =>
      circe.jawn.decode[User](userJson) match {
        case Right(user) =>
          val result = startUser(user)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case DeleteUser(id) => sender() ! deleteUser(id).asJson.noSpaces
    case EditUser(userJson, id) =>
      circe.jawn.decode[User](userJson) match {
        case Right(user) =>
          val result = editUser(user, id)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case EditUsersProject(idUsers, idProject) =>
      circe.jawn.decode[List[Int]](idUsers) match {
        case Right(arrayOfUsers) =>
          val result = editUsersProject(arrayOfUsers, idProject)
          sender() ! result.asJson.noSpaces
        case _ => sender() ! "error".asJson.noSpaces
      }
    case JoinUsersProjects() => sender() ! joinUsersProjects.asJson.noSpaces
    case GetUsersProject(id) => sender() ! getUsersProject(id).asJson.noSpaces
    case GetUserVisibleProjects(id) => sender() ! getUserVisibleProjects(id).asJson.noSpaces
    case SendLogPass(id) => sender() ! sendLogPass(id).asJson.noSpaces
    case ShareRights(user, with_user) =>
      shareWith(user, with_user)
      sender() ! ("success").asJson.noSpaces
    case UpdateEmail(user, email) =>
      updateEmail(user, email)
      sender() ! ("success").asJson.noSpaces
    case UpdateRocketLogin(user, rocketLogin) =>
      updateRocketLogin(user, rocketLogin)
      sender() ! ("success").asJson.noSpaces
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
