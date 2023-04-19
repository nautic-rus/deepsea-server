package deepsea.auth

import deepsea.actors.ActorManager
import deepsea.auth.AuthManager.{AdminRight, Department, Page, RightUser, Role, User, UserProject}
import deepsea.database.{DBManager, MongoCodecs}
import deepsea.issues.{IssueManager, IssueManagerHelper}
import deepsea.issues.IssueManager.IssueProject
import deepsea.mail.MailManager.Mail
import deepsea.rocket.RocketChatManager.SendNotification

import scala.collection.mutable.ListBuffer
import scala.io.Source

trait AuthManagerHelper extends MongoCodecs with IssueManagerHelper {
  def getUser(login: String): Option[User] = {
    val usersProjects = getUsersProjects
    val userRights = getRights
    val roles = getRoles
    DBManager.GetPGConnection() match {
      case Some(c) =>
        var res = Option.empty[User]
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users where login = '$login' and removed = 0")
        while (rs.next()) {
          val userId = Option(rs.getInt("id")).getOrElse(0)
          val groups = rs.getString("group").split(",").toList
          res = Option(User(
            userId,
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
            usersProjects.filter(_.user_id == userId.toString).map(x => x.project_name),
            rs.getString("visible_pages").split(",").toList,
            rs.getString("shared_access").split(",").toList,
            groups,
            roles.find(x => groups.contains(x)) match {
              case Some(role) => role.rights
              case _ => userRights.filter(_.userId == userId).map(_.role)
            },
            "",
            rs.getInt("id_department")
          ))
        }
        rs.close()
        s.close()
        c.close()
//        if (res.nonEmpty) {
//          s = c.createStatement()
//          rs = s.executeQuery(s"select rights from user_rights where user_id = ${res.get.id}")
//          val permissions = ListBuffer.empty[String]
//          while (rs.next()) {
//            permissions += rs.getString("rights")
//          }
//          res.get.permissions = permissions.toList
//          rs.close()
//          s.close()
//        }
        res
      case _ => Option.empty[User]
    }
  }

  def getUsers: List[User] = {
    val res = ListBuffer.empty[User]
    val userRights = getRights
    val usersProjects = getUsersProjects
    val roles = getRoles
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from users where removed = 0 order by id")
        while (rs.next()) {
          val userId = Option(rs.getInt("id")).getOrElse(0)
          val groups = rs.getString("group").split(",").toList
          res += User(
            userId,
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
            usersProjects.filter(x => {
              x.user_id.equals(userId.toString)
            }).map(x => x.project_name),
            rs.getString("visible_pages").split(",").toList,
            rs.getString("shared_access").split(",").toList,
            rs.getString("group").split(",").toList,
            roles.find(x => groups.contains(x)) match {
              case Some(role) => role.rights
              case _ => userRights.filter(_.userId == userId).map(_.role)
            },
            "",
            rs.getInt("id_department")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }

  def getRightDetails(id: Int): ListBuffer[String] = {
    val res = ListBuffer.empty[String];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from user_rights where user_id = $id");
        while (rs.next()) {
          res += rs.getString("rights")
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[String]
    }
  }

  def updateEmail(login: String, email: String): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update users set email = '$email' where login = '$login'")
        s.close()
        c.close()
      case _ => None
    }
  }

  def updateRocketLogin(login: String, rocket: String): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update users set rocket_login = '$rocket' where login = '$login'")
        s.close()
        c.close()
      case _ => None
    }
  }

  def getUserDetails(id: String): Option[User] = {
    var user: Option[User] = Option.empty[User]
    val usersProjects = getUsersProjects
    val userRights = getRights
    val roles = getRoles
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users where id = '$id'")
        while (rs.next()) {
          val userId = Option(rs.getInt("id")).getOrElse(0)
          val groups = rs.getString("group").split(",").toList
          user = Option(User(
            userId,
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
            usersProjects.filter(_.user_id == id).map(x => x.project_name),
            rs.getString("visible_pages").split(",").toList,
            rs.getString("shared_access").split(",").toList,
            groups,
            roles.find(x => groups.contains(x)) match {
              case Some(role) => role.rights
              case _ => userRights.filter(_.userId == userId).toList.map(_.role)
            },
            "",
            rs.getInt("id_department")))
        }
        rs.close()
        s.close()
        c.close()
        user
      case _ => Option.empty[User]
    }
  }

  def startUser(user: User): String = {
    var userId = user.id;
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val q = '"'
        val query = s"insert into users (id, login, password, name, surname, birthday, email, phone, tcid, avatar, profession, visibility, gender, avatar_full, department, rocket_login, visible_projects, ${q}group${q}) " +
          s"values (default, '${user.login}', '${user.password}', '${user.name}', '${user.surname}', '${user.birthday}', '${user.email}', '${user.phone}', ${user.tcid}, '${user.avatar}', '${user.profession}', '${user.visibility}', '${user.gender}', '${user.avatar_full}', '${user.department}', '${user.rocket_login}', '${user.visible_projects.mkString(",")}', '${user.groups.mkString(",")}')" +
          s" returning id"
        val rs = s.executeQuery(query);
        while (rs.next()) {
          userId = rs.getInt("id");
        }

        if (user.permissions.nonEmpty) {
          user.permissions.foreach(role => s.execute(s"insert into user_rights (user_id, rights) values ('$userId', '$role')"))
        }
        if (user.visible_projects.nonEmpty) {
          s.execute(s"delete from users_visibility_projects where user_id = '$userId'");
          val projects: List[IssueManager.IssueProject] = getIssueProjects.toList;
          user.visible_projects.foreach(uProject => {
            projects.filter(x => x.name == uProject).map(project => {
              val queryProject = s"insert into users_visibility_projects (user_id, project_id) values ('$userId', '${project.id}')";
              s.execute(queryProject);
            })
          })
        }
        rs.close()
        s.close()
        c.close()
        val messageRocket: String = s"Ваши данные для входа в DeepSea - Логин: ${user.login} | Пароль: ${user.password} | https://deep-sea.ru";
        val messageMail: String = Source.fromResource("messages/startUser.html").mkString.replaceAll("!login", s"${user.login}").replaceAll("!password", s"${user.password}")
        ActorManager.rocket ! SendNotification(user.rocket_login, messageRocket);
        ActorManager.mail ! Mail(List(user.name, user.surname).mkString(" "), user.email, "DeepSea Notification", messageMail);
        "success"
      case _ => "error"
    }
  }

  def sendLogPass(id: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"select * from users where id = $id";
        val rs = s.executeQuery(query);
        while (rs.next()) {
          val login = rs.getString("login");
          val password = rs.getString("password");
          val name = rs.getString("name");
          val surname = rs.getString("surname");
          val rocket_login = rs.getString("rocket_login");
          val email = rs.getString("email");
          val messageRocket: String = s"Ваши данные для входа в DeepSea - Логин: ${login} | Пароль: ${password} | https://deep-sea.ru";
          val messageMail: String = Source.fromResource("messages/sendLogPass.html").mkString.replaceAll("!login", s"${login}").replaceAll("!password", s"${password}")
          ActorManager.rocket ! SendNotification(rocket_login, messageRocket);
          ActorManager.mail ! Mail(List(name, surname).mkString(" "), email, "DeepSea Notification", messageMail);
        };
        rs.close()
        s.close()
        c.close()
        "success";
      case _ => "error"
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

  def editUser(user: User, id: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val q = '"'
        val query = s"update users set id = '${user.id}', login = '${user.login}', password = '${user.password}', name = '${user.name}', surname = '${user.surname}', email = '${user.email}', phone = '${user.phone}', tcid = '${user.tcid}', avatar = '${user.avatar}', profession = '${user.profession}', visibility = '${user.visibility}', gender = '${user.gender}', department = '${user.department}', visible_projects = '${user.visible_projects.mkString(",")}', rocket_login = '${user.rocket_login}', ${q}group${q} = '${user.groups.mkString(",")}', id_department = '${user.id_department}' where id = '$id'"
        s.execute(query);

        s.execute(s"delete from user_rights where user_id = '$id'");
        user.permissions.foreach(role => {
          val query = s"insert into user_rights (user_id, rights) values ('$id', '$role')";
          s.execute(query);
        })

        s.execute(s"delete from users_visibility_projects where user_id = '$id'");
        val projects: List[IssueManager.IssueProject] = getIssueProjects.toList;
        user.visible_projects.foreach(uProject => {
          projects.filter(x => x.name == uProject).map(project => {
            val queryProject = s"insert into users_visibility_projects (user_id, project_id) values ('${user.id}', '${project.id}')";
            s.execute(queryProject);
          })
        })
        s.close();
        c.close();
        "success"
      case _ => "error";
    }
  }

  def editUsersProject(idUsers: List[Int], idProject: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val delQuery = s"delete from users_visibility_projects where project_id = '$idProject'";
        s.execute(delQuery);
        idUsers.foreach(id => {
          val insertQuery = s"insert into users_visibility_projects (user_id, project_id) values ('$id', '$idProject')";
          s.execute(insertQuery);
          val project: IssueManager.IssueProject = getProjectDetails(idProject).get;
          val user: User = getUserDetails(id.toString).get;
          val userProjects = user.visible_projects.appended(project.name);
          val updateQuery = s"update users set visible_projects = '${userProjects.mkString(",")}' where id = '$id'";
          s.execute(updateQuery);
        })
        s.close()
        c.close()
        "success"
      case _ => "error"
    }
  }

  def joinUsersProjects(): String = {
//    DBManager.GetPGConnection() match {
//      case Some(c) =>
//        val s = c.createStatement();
//        s.execute("delete from users_visibility_projects");
//        val users: List[User] = getUsers
//        val projects: List[IssueProject] = getIssueProjects.toList
//        users.foreach(user => {
//          user.visible_projects.foreach(uProject => {
//            projects.foreach(project => {
//              if (uProject == project.name) {
//                val queryProject = s"insert into users_visibility_projects (user_id, project_id) values ('${user.id}', '${project.id}')";
//                s.execute(queryProject);
//              }
//            })
//          })
//        })
//        s.close()
//        c.close()
//        "success"
//      case _ => "error"
//    }
    "success"
  }

  def getUserVisibleProjects(id: String): List[String] = {
    getUsersProjects.filter(_.user_id == id).map(_.project_name)
  }

  def getUsersProject(id: String): List[Int] = {
    val res = ListBuffer.empty[Int];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"select user_id from users_visibility_projects where project_id = '$id'"
        val rs = s.executeQuery(query);
        while (rs.next()) {
          res.append(rs.getInt("user_id"))
        }
        rs.close()
        s.close()
        c.close()
        res.toList
      case _ => List.empty[Int]
    }
  }

  def getUsersProjects: List[UserProject] = {
    val res: ListBuffer[UserProject] = ListBuffer.empty[UserProject];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"select uvp.user_id, ip.name from users_visibility_projects uvp, issue_projects ip where uvp.project_id = ip.id"
        val rs = s.executeQuery(query)
        while (rs.next()) {
          res += (UserProject(
            rs.getString("user_id"),
            rs.getString("name")
          ))
        }
        rs.close()
        s.close()
        c.close()
        res.toList
      case _ => List.empty[UserProject]
    }
  }


  def getAdminRights: ListBuffer[AdminRight] = {
    val res = ListBuffer.empty[AdminRight]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery("select * from rights")
        while (rs.next()) {
          res += AdminRight(
            rs.getString("name")
          )
        }
        rs.close()
        s.close()
        c.close()
        res
      case _ => ListBuffer.empty[AdminRight]
    }
  }

  def getAdminRightsDetails(name: String): Option[AdminRight] = {
    var right: Option[AdminRight] = Option.empty[AdminRight]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from rights where name = '$name'")
        while (rs.next()) {
          right = Option(AdminRight(
            rs.getString("name")
          ))
        }
        rs.close()
        s.close()
        c.close()
        right
      case _ => Option.empty[AdminRight]
    }
  }

  def startRight(right: AdminRight): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"insert into rights (name) values ('${right.name}')";
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }

  def deleteAdminRight(name: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"delete from rights where name = '$name'"
        s.execute(query)
        s.close()
        c.close()
        "success"
      case _ => "error"
    }
  }

  def editAdminRight(right: AdminRight, name: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = s"update rights set name = '${right.name}' where name = '$name'"
        s.execute(query)
        s.close()
        c.close()
        "success"
      case _ => "error"
    }

  }

  def getRoles: List[Role] = {
    val res = ListBuffer.empty[Role]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from roles")
        while (rs.next()) {
          res += Role(
            rs.getString("name"),
            rs.getString("description"),
            rs.getString("rights").split(",").toList,
          )
        }
        rs.close()
        s.close()
        c.close()
        res.toList
      case _ => List.empty[Role]
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
            rs.getString("rights").split(",").toList
          ))
        }
        rs.close()
        s.close()
        c.close()
        role
      case _ => Option.empty[Role]
    }
  }

  def getRoleRights(name: String): List[String] = {
    var rights: List[String] = List.empty[String]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select rights from roles where name = '$name'");
        while (rs.next()) {
          rights = rs.getString("rights").split(",").toList
        }
        rs.close()
        s.close()
        c.close()
        rights
      case _ => List.empty[String]
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
        val query = s"insert into roles (name, description, rights) values ('${role.name}', '${role.description}', '${role.rights.mkString(",")}')";
        s.execute(query);
        s.close();
        c.close();
        "success";
      case _ => "error";
    }
  }

  def saveRoleForAll(name: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        var id_users = ListBuffer.empty[String]
        val s = c.createStatement()
        val q = '"'
        val query = s"select id from users where ${q}group$q like '%$name%'"
        val rs = s.executeQuery(query)
        while (rs.next()) {
          id_users += rs.getString("id")
        }
        val roles = getRoles
        id_users.foreach(id => {
          s.execute(s"delete from user_rights where user_id = '$id'")
          roles.find(_.name == name) match {
            case Some(role) =>
              role.rights.filter(x => x != "").foreach(rights => {
                val query = s"insert into user_rights (user_id, rights) values ('$id', '$rights')";
                s.execute(query)
              })
            case _ => None
          }
        })
        rs.close();
        s.close();
        c.close();
        "success"
      case _ => "error"
    }
  }


  def editRole(role: Role, name: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val query = s"update roles set name = '${role.name}', description = '${role.description}', rights = '${role.rights.mkString(",")}' where name = '$name'";
        s.execute(query);
        //        val queryR = s"update user_rights set rights = '${role.name}' where rights = '$name'";
        //        s.execute(queryR);
        s.close();
        c.close();
        "success"
      case _ => "error";
    }
  }

  def getPages: List[Page] = {
    val res = ListBuffer.empty[Page]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from pages")
        while (rs.next()) {
          res += Page(
            rs.getInt("id"),
            rs.getString("name")
          )
        }
        rs.close()
        s.close()
        c.close()
        res.toList
      case _ => List.empty[Page]
    }
  }

  def getRights: List[RightUser] = {
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
        rs.close()
        s.close()
        c.close()
        res.toList
      case _ => List.empty[RightUser]
    }
  }

  def getDepartments: List[Department] = {
    val res = ListBuffer.empty[Department];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from issue_departments order by id");
        while (rs.next()) {
          res += Department(
            rs.getInt("id"),
            rs.getString("name"),
            rs.getString("manager"),
            rs.getInt("visible_documents"),
            rs.getInt("visible_man_hours"),
          )
        }
        rs.close()
        s.close()
        c.close()
        res.toList
      case _ => List.empty[Department]
    }
  }

  def getDepartmentDetail(id: String): Option[Department] = {
    var department: Option[Department] = Option.empty[Department]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_departments where id = '$id'")
        while (rs.next()) {
          department = Option(Department(
            rs.getInt("id"),
            rs.getString("name"),
            rs.getString("manager"),
            rs.getInt("visible_documents"),
            rs.getInt("visible_man_hours"),
          ))
        }
        rs.close()
        s.close()
        c.close()
        department
      case _ => Option.empty[Department]
    }
  }

  def notifySubscribers(issue: Int, email: String, rocket: String): Unit = {
    getIssueSubscribers(issue).foreach(s => {
      getUser(s.user) match {
        case Some(user) =>
          s.options.split(",").foreach {
            case "mail" => ActorManager.mail ! Mail(List(user.surname, user.name).mkString(" "), user.email, "DeepSea Notification", rocket)
            case "rocket" => ActorManager.rocket ! SendNotification(s.user, email)
            case _ => None
          }
        case _ => None
      }
    })
  }
}
