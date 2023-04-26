package deepsea.fest

import akka.actor.Actor
import deepsea.database.{DBManager, MongoCodecs}
import deepsea.fest.FestManager._
import deepsea.fest.classes.{BestPlayer, Mark, TeamWon}
import deepsea.files.FileManager.{TreeFile, treeFilesCollection}
import deepsea.files.classes.FileAttachment
import io.circe.syntax.EncoderOps
import org.mongodb.scala.MongoCollection
import io.circe.parser.decode
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

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

  case class SetFestSauna(kind: String, users: String, time: String)
  case class GetFestSauna()
  case class DeleteFestSauna(time: String)

  case class FestStories(url: String, thumb: String, date: Long)

  case class FestKaraoke(users: String, song: String, date: Long)

  case class FestSauna(kind: String, users: String, time: String)
}
class FestManager extends Actor with MongoCodecs{
  override def receive: Receive = {

    case GetMarks() => sender() ! getFestMarks.asJson.noSpaces
    case SetMarks(marksValue) =>
      decode[List[Mark]](marksValue) match {
        case Right(marks) =>
          updateMarks(marks)
          sender() ! ("success").asJson.noSpaces
        case Left(value) =>
          sender() ! ("error").asJson.noSpaces
      }


    case GetTeamsWon() => sender() ! getTeamsWon.asJson.noSpaces
    case SetTeamsWon(teamsValue) =>
      decode[List[TeamWon]](teamsValue) match {
        case Right(teams) =>
          updateTeamsWon(teams)
          sender() ! ("success").asJson.noSpaces
        case Left(value) =>
          sender() ! ("error").asJson.noSpaces
      }
    case GetBestPlayers() => sender() ! getBestPlayers.asJson.noSpaces
    case SetBestPlayer(bestPlayerValue) =>
      decode[BestPlayer](bestPlayerValue) match {
        case Right(player) =>
          updateBestPlayer(player)
          sender() ! ("success").asJson.noSpaces
        case Left(value) =>
          sender() ! ("error").asJson.noSpaces
      }
    case GetFestStories() =>
      sender() ! getFestStories.asJson.noSpaces
    case SetFestStories(url, thumb) =>
      setFestStories(url, thumb)
      sender() ! "success".asJson.noSpaces
    case DeleteFestStories(url) =>
      deleteFestStories(url)
      sender() ! "success".asJson.noSpaces
    case GetFestKaraoke() =>
      sender() ! getFestKaraoke.asJson.noSpaces
    case SetFestKaraoke(users, song) =>
      setFestKaraoke(users, song)
      sender() ! "success".asJson.noSpaces
    case DeleteFestKaraoke(date) =>
      deleteFestKaraoke(date.toLongOption.getOrElse(0))
      sender() ! "success".asJson.noSpaces
    case GetFestSauna() =>
      sender() ! getFestSauna.asJson.noSpaces
    case SetFestSauna(kind, users, time) =>
      setFestSauna(kind, users, time)
      sender() ! "success".asJson.noSpaces
    case DeleteFestSauna(time) =>
      deleteFestSauna(time)
      sender() ! "success".asJson.noSpaces
    case _ => None
  }

  def updateMarks(marks: List[Mark]): Unit ={
    val current = getFestMarks
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        marks.foreach(mark => {
          if (current.exists(x => x.team == mark.team && x.disc == mark.disc)){
            s.execute(s"update fest_marks set value = ${mark.value} where team = '${mark.team}' and disc = '${mark.disc}'")
          }
          else {
            s.execute(s"insert into fest_marks values ('${mark.disc}', '${mark.team}', ${mark.value})")
          }
        })
        s.close()
        c.close()
      case _ =>
    }
  }
  def getFestMarks: ListBuffer[Mark] ={
    val res = ListBuffer.empty[Mark]
    DBManager.GetPGFestConnection() match {
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
    DBManager.GetPGFestConnection() match {
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
  def updateTeamsWon(teams: List[TeamWon]): Unit ={
    val current = getTeamsWon
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        teams.foreach(team => {
          if (current.exists(x => x.game == team.game && x.sport == team.sport)){
            s.execute(s"update fest_teams_won set team = '${team.team}' where sport = '${team.sport}' and game = '${team.game}'")
          }
          else{
            s.execute(s"insert into fest_teams_won values ('${team.sport}', '${team.game}', '${team.team}')")
          }
        })
        s.close()
        c.close()
      case _ =>
    }
  }
  def getBestPlayers: ListBuffer[BestPlayer] ={
    val res = ListBuffer.empty[BestPlayer]
    DBManager.GetPGFestConnection() match {
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
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        if (current.exists(x => x.disc == player.disc)){
          s.execute(s"update fest_best_players set name = '${player.name}' where disc = '${player.disc}'")
        }
        else{
          s.execute(s"insert into fest_best_players values ('${player.name}', '${player.disc}')")
        }
        s.close()
        c.close()
      case _ =>
    }
  }

  def getFestStories: ListBuffer[FestStories] ={
    val res = ListBuffer.empty[FestStories]
    DBManager.GetPGFestConnection() match {
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
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into fest_stories values ('${url}', '${thumb}', $date)")
        c.close()
      case _ =>
    }
  }
  def deleteFestStories(url: String): Unit ={
    DBManager.GetPGFestConnection() match {
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
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_karaoke")
        while (rs.next()){
          res += FestKaraoke(
            rs.getString("users"),
            rs.getString("song"),
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
    DBManager.GetPGFestConnection() match {
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
    DBManager.GetPGFestConnection() match {
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
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from fest_sauna")
        while (rs.next()){
          res += FestSauna(
            rs.getString("kind"),
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
  def setFestSauna(kind: String, users: String, time: String): Unit ={
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val date = new Date().getTime
        s.execute(s"insert into fest_sauna values ('${users}', '${time}',  ${date}, '${kind}')")
        s.close()
        c.close()
      case _ =>
    }
  }
  def deleteFestSauna(time: String): Unit ={
    DBManager.GetPGFestConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from fest_sauna where time = ${time}")
        s.close()
        c.close()
      case _ =>
    }
  }
}
