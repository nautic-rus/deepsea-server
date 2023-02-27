package deepsea.actors

import deepsea.actors.ActorStartupManager.Start
import akka.actor.{ActorRef, ActorSystem, Props}

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object ActorManager {

  var system: ActorSystem = _
  var startup: ActorRef = _
  var httpServer: ActorRef = _
  var dataBase: ActorRef = _
  var auth: ActorRef = _
  var issue: ActorRef = _
  var files: ActorRef = _
  var mail: ActorRef = _
  var materials: ActorRef = _
  var rocket: ActorRef = _
  var rest: ActorRef = _
  var license: ActorRef = _
  var timeControl: ActorRef = _
  var timeAndWeather: ActorRef = _
  var fest: ActorRef = _
  var mobile: ActorRef = _
  var backups: ActorRef = _
  var osmManager: ActorRef = _
  var planHours: ActorRef = _

  def init(): Unit ={
    system = ActorSystem()
    startup = system.actorOf(Props[ActorStartupManager])
    startup ! Start()
  }
  def initSchedulers(): Unit ={

  }
  def terminate(): Unit ={
    system.terminate()
    Await.ready(system.whenTerminated, Duration(30, TimeUnit.SECONDS))
  }
}
