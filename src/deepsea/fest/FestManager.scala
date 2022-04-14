package deepsea.fest

import akka.actor.Actor
import deepsea.database.DatabaseManager.GetConnection
import deepsea.fest.FestManager._
import deepsea.fest.classes.{BestPlayer, Mark, TeamWon}
import deepsea.files.classes.FileAttachment
import play.api.libs.json.{Json, OWrites}

import java.util.Date
import scala.collection.mutable.ListBuffer

object FestManager{

  case class GetMarks()
  case class SetMarks(marks: String)

  case class GetTeamsWon()
  case class SetTeamsWon(teams: String)

  case class GetBestPlayers()
  case class SetBestPlayer(player: String)

  case class SetFestStories(url: String, thumb: String)
  case class GetFestStories()
  case class DeleteFestStories(url: String)

  case class SetFestKaraoke(users: String, song: String)
  case class GetFestKaraoke()
  case class DeleteFestKaraoke(date: String)

  case class SetFestSauna(users: String, time: String)
  case class GetFestSauna()
  case class DeleteFestSauna(time: String)

  case class FestStories(url: String, thumb: String, date: Long)
  implicit val writesFestStories: OWrites[FestStories] = Json.writes[FestStories]

  case class FestKaraoke(users: String, song: String, date: Long)
  implicit val writesFestKaraoke: OWrites[FestKaraoke] = Json.writes[FestKaraoke]

  case class FestSauna(users: String, time: String)
  implicit val writesFestSauna: OWrites[FestSauna] = Json.writes[FestSauna]
}
class FestManager extends Actor{
  override def receive: Receive = {

    case GetMarks() => sender() ! Json.toJson(getFestMarks)
    case SetMarks(marksValue) =>
      Json.parse(marksValue).asOpt[ListBuffer[Mark]] match {
        case Some(marks) =>
          updateMarks(marks)
          sender() ! Json.toJson("success")
        case _ =>
          sender() ! Json.toJson("error")
      }

    case GetTeamsWon() => sender() ! getTeamsWon
    case SetTeamsWon(teams) =>
      Json.parse(teams).asOpt[ListBuffer[TeamWon]] match {
        case Some(teams) =>
          updateTeamsWon(teams)
          sender() ! Json.toJson("success")
        case _ =>
          sender() ! Json.toJson("error")
      }

    case GetBestPlayers() => sender() ! getBestPlayers
    case SetBestPlayer(player) =>
      Json.parse(player).asOpt[BestPlayer] match {
        case Some(player) =>
          updateBestPlayer(player)
          sender() ! Json.toJson("success")
        case _ =>
          sender() ! Json.toJson("error")
      }

    case GetFestStories() =>
      sender() ! Json.toJson(getFestStories)
    case SetFestStories(url, thumb) =>
      setFestStories(url, thumb)
      sender() ! Json.toJson("success")
    case DeleteFestStories(url) =>
      deleteFestStories(url)
      sender() ! Json.toJson("success")

    case GetFestKaraoke() =>
      sender() ! Json.toJson(getFestKaraoke)
    case SetFestKaraoke(users, song) =>
      setFestKaraoke(users, song)
      sender() ! Json.toJson("success")
    case DeleteFestKaraoke(date) =>
      deleteFestKaraoke(date.toLongOption.getOrElse(0))
      sender() ! Json.toJson("success")

    case GetFestSauna() =>
      sender() ! Json.toJson(getFestSauna)
    case SetFestSauna(users, time) =>
      setFestSauna(users, time)
      sender() ! Json.toJson("success")
    case DeleteFestSauna(time) =>
      deleteFestSauna(time)
      sender() ! Json.toJson("success")

    case _ => None
  }

  def updateMarks(marks: ListBuffer[Mark]): Unit ={
    val current = getFestMarks
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        marks.foreach(mark => {
          if (current.exists(x => x.team == mark.team && x.disc == mark.disc)){
            s.execute(s"update fest_marks set value = ${mark.value} where team = ${mark.team} and disc = ${mark.disc}")
          }
          else {
            s.execute(s"insert into fest_marks values (${mark.disc}, ${mark.team}, ${mark.value})")
          }
        })
        s.close()
        c.close()
      case _ =>
    }
  }
  def getFestMarks: ListBuffer[Mark] ={
    val res = ListBuffer.empty[Mark]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_marks")
        while (rs.next()){
          res += new Mark(rs.getString("disc"), rs.getString("team"), rs.getDouble("value"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getTeamsWon: ListBuffer[TeamWon] ={
    val res = ListBuffer.empty[TeamWon]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_teams_won")
        while (rs.next()){
          res += new TeamWon(rs.getString("sport"), rs.getString("game"), rs.getString("team"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def updateTeamsWon(teams: ListBuffer[TeamWon]): Unit ={
    val current = getTeamsWon
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        teams.foreach(team => {
          if (current.exists(x => x.game == team.game && x.sport == team.sport)){
            s.execute(s"update fest_teams_won set team = ${team.team} where sport = ${team.sport} and game = ${team.game}")
          }
          else{
            s.execute(s"insert into fest_teams_won values (${team.sport}, ${team.game}, ${team.team})")
          }
        })
        s.close()
        c.close()
      case _ =>
    }
  }
  def getBestPlayers: ListBuffer[BestPlayer] ={
    val res = ListBuffer.empty[BestPlayer]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_best_players")
        while (rs.next()){
          res += new BestPlayer(rs.getString("name"), rs.getString("disc"))
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def updateBestPlayer(player: BestPlayer): Unit ={
    val current = getBestPlayers
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        if (current.exists(x => x.disc == player.disc)){
          s.execute(s"update fest_best_players set name = ${player.name} where disc = ${player.disc}")
        }
        else{
          s.execute(s"insert into fest_best_players values (${player.name}, ${player.disc})")
        }
        s.close()
        c.close()
      case _ =>
    }
  }

  def getFestStories: ListBuffer[FestStories] ={
    val res = ListBuffer.empty[FestStories]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_stories")
        while (rs.next()){
          res += FestStories(
            rs.getString("url"),
            rs.getString("thumb"),
            rs.getLong("date"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setFestStories(url: String, thumb: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into fest_stories values ('${url}', '${thumb}', $date)")
        c.close()
      case _ =>
    }
  }
  def deleteFestStories(url: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"delete from fest_stories where url = '${url}'")
        c.close()
      case _ =>
    }
  }

  def getFestKaraoke: ListBuffer[FestKaraoke] ={
    val res = ListBuffer.empty[FestKaraoke]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_karaoke")
        while (rs.next()){
          res += FestKaraoke(
            rs.getString("song"),
            rs.getString("users"),
            rs.getLong("date"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setFestKaraoke(users: String, song: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into fest_karaoke values ('${song}', '${users}',  ${date})")
        s.close()
        c.close()
      case _ =>
    }
  }
  def deleteFestKaraoke(date: Long): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from fest_karaoke where date = ${date}")
        s.close()
        c.close()
      case _ =>
    }
  }

  def getFestSauna: ListBuffer[FestSauna] ={
    val res = ListBuffer.empty[FestSauna]
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_sauna")
        while (rs.next()){
          res += FestSauna(
            rs.getString("users"),
            rs.getString("time")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def setFestSauna(users: String, time: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into fest_sauna values ('${users}', '${time}',  ${date})")
        s.close()
        c.close()
      case _ =>
    }
  }
  def deleteFestSauna(time: String): Unit ={
    GetConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"delete from fest_sauna where time = ${time}")
        s.close()
        c.close()
      case _ =>
    }
  }
}
