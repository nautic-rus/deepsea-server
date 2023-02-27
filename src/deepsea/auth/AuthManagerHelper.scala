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
            rs.getString("group").split(",").toList,
            List.empty[String],
            "",
            rs.getString("projects").split(",").toList
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
  def getUsers: ListBuffer[User] = {
    val res = ListBuffer.empty[User]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from users where removed = 0")
        while (rs.next()) {
          val id = Option(rs.getInt("id")).getOrElse(0)
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
            getRightDetails(id).toList,
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
  def getRightDetails(id: Int): ListBuffer[String] = {
    val res = ListBuffer.empty[String];
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement();
        val rs = s.executeQuery(s"select * from user_rights where user_id = $id");
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
