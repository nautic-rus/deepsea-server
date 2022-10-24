package deepsea

import deepsea.actors.ActorManager

import scala.io.StdIn.readLine

object App {
  object HTTPServer{
    val Host1 = "192.168.1.28"
    val Host = "192.168.1.122"
    val Port = 1112
    val Url = "https://deep-sea.ru"
    val RestUrl = "https://deep-sea.ru/rest"
    val FestRestUrl = "https://fest.nautic-rus.com/rest"
  }
  object Cloud{
    val Host = "cloud.nautic-rus.com"
    val Host1 = "ncloud.nautic-rus.com"
    val Protocol = "https"
    val Directory = "/cloud"
    val Url: String = App.HTTPServer.RestUrl + "/files"
    val FestUrl: String = App.HTTPServer.FestRestUrl + "/files"
    val UserName: String = "op"
    val Password1: String = "5ZMqdqMc"
    val Password: String = "5ZMqdqMc5ZMqdqMc"
  }
  def main(args: Array[String]): Unit = {
    org.apache.log4j.BasicConfigurator.configure()
    ActorManager.init()
    while (readLine() != "q") {}
    ActorManager.terminate()
  }
}
