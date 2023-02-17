package deepsea.auth

import deepsea.auth.AuthManager.User
import deepsea.database.DBManager

import scala.collection.mutable.ListBuffer

trait AuthManagerHelper {
  def getUser(login: String): Option[User] ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        var s = c.createStatement()
        var rs = s.executeQuery(s"select * from users where login = '$login' and removed = 0")
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
            rs.getString("group").split(",").toList
          ))
        }
        s.close()
        if (res.nonEmpty){
          s = c.createStatement()
          //          rs = s.executeQuery(s"select type_name from issue_types where id in (select group_id from user_membership where user_id = ${res.get.id})")
          //          while (rs.next()){
          //            res.get.groups += rs.getString("type_name")
          //          }
          //          s.close()
          //          s = c.createStatement()
          rs = s.executeQuery(s"select rights from user_rights where user_id = ${res.get.id}")
          val permissions = ListBuffer.empty[String]
          while (rs.next()){
            permissions += rs.getString("rights")
          }
          res.get.permissions = permissions.toList
          rs.close()
          s.close()
        }
        c.close()
        res
      case _ => Option.empty[User]
    }
  }
  def updateEmail(login: String, email: String): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update users set email = '$email' where login = '$login'")
        s.close()
        c.close()
      case _ => None
    }
  }
  def updateRocketLogin(login: String, rocket: String): Unit ={
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"update users set rocket_login = '$rocket' where login = '$login'")
        s.close()
        c.close()
      case _ => None
    }
  }
}
