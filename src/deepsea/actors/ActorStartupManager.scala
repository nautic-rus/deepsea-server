package deepsea.actors

import akka.actor.{Actor, Props}
import akka.routing.RoundRobinPool
import deepsea.actors.ActorManager.system
import deepsea.actors.ActorStartupManager.{DatabaseManagerStarted, HTTPManagerStarted, Start}
import deepsea.auth.AuthManager
import deepsea.database.DatabaseManager
import deepsea.fest.FestManager
import deepsea.files.FileManager
import deepsea.http.HTTPManager
import deepsea.issues.IssueManager
import deepsea.mail.MailManager
import deepsea.materials.MaterialManager
import deepsea.mobile.MobileManager
import deepsea.rocket.RocketChatManager
import deepsea.time.{LicenseManager, TimeAndWeatherManager, TimeControlManager}


object ActorStartupManager{
  case class Start()
  case class DatabaseManagerStarted()
  case class HTTPManagerStarted()
}
class ActorStartupManager extends Actor{
  override def receive: Receive = {
    case Start() =>
      ActorManager.dataBase = system.actorOf(Props[DatabaseManager])
    case DatabaseManagerStarted() =>
      ActorManager.httpServer = system.actorOf(RoundRobinPool(1).props(Props[HTTPManager]))
    case HTTPManagerStarted() =>
      ActorManager.auth = system.actorOf(RoundRobinPool(10).props(Props[AuthManager]))
      ActorManager.issue = system.actorOf(RoundRobinPool(10).props(Props[IssueManager]))
      ActorManager.files = system.actorOf(RoundRobinPool(10).props(Props[FileManager]))
      ActorManager.mail = system.actorOf(RoundRobinPool(1).props(Props[MailManager]))
      ActorManager.materials = system.actorOf(RoundRobinPool(1).props(Props[MaterialManager]))
      ActorManager.rocket = system.actorOf(RoundRobinPool(1).props(Props[RocketChatManager]))
      ActorManager.license = system.actorOf(RoundRobinPool(1).props(Props[LicenseManager]))
      ActorManager.timeControl = system.actorOf(RoundRobinPool(5).props(Props[TimeControlManager]))
      ActorManager.timeAndWeather = system.actorOf(RoundRobinPool(1).props(Props[TimeAndWeatherManager]))
      ActorManager.fest = system.actorOf(RoundRobinPool(3).props(Props[FestManager]))
      ActorManager.mobile = system.actorOf(RoundRobinPool(1).props(Props[MobileManager]))
  }
}
