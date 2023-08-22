package deepsea.time

import akka.actor.Actor
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import deepsea.time.LicenseManager.GetForanLicenses
import org.jsoup.Jsoup

import scala.collection.mutable.ListBuffer
import scala.sys.process.Process

object LicenseManager{
  case class GetForanLicenses()
}
class LicenseManager extends Actor{
  override def receive: Receive = {
    case GetForanLicenses() =>
      sender() ! getForanLicenses()
    case _ => None
  }
  def getForanLicenses(): HttpEntity.Strict = {
//    val foranLicense = Process("\\\\nautic-dc.nautic.rus\\configs\\LMTools\\lmutil.exe lmstat -a -c 27001@192.168.1.21").!!
//    var readForan = false
//    var readThinkDesign = false
//    var readFCM = false
//    val foranRows = ListBuffer.empty[String]
//    val thinkDesignRows = ListBuffer.empty[String]
//    val fcmRows = ListBuffer.empty[String]
//    foranLicense.lines().forEach(line => {
//      if (line.contains("Users of GP01")){
//        readForan = true
//      }
//      if (line.contains("Users of GP01_0")){
//        readForan = false
//      }
//      if (readForan){
//        foranRows += line
//      }
//      if (line.contains("Users of FD1")){
//        readThinkDesign = true
//      }
//      if (line.contains("Users of FC1:")){
//        readThinkDesign = false
//      }
//      if (readThinkDesign){
//        thinkDesignRows += line
//      }
//      if (line.contains("Users of FCM")){
//        readFCM = true
//      }
//      if (line.contains("Users of FDESIGN:")){
//        readFCM = false
//      }
//      if (readFCM){
//        fcmRows += line
//      }
//    })

    val rhinoRows = ListBuffer.empty[String]
    var row = ""
    var count = 0
    Jsoup.connect("http://192.168.1.21/status").get().body().getElementsByTag("td").forEach(x => {
      val text = x.text()
      count += 1
      row += text + " - "
      if (count == 8){
        count = 0
        rhinoRows += row.trim.dropRight(2)
        row = ""
      }
    })

    var res = ""
//    res += "FORAN LICENSES"
//    foranRows.foreach(x => res += "<br>" + x)
//    res += "<br>THINK DESIGN LICENSES"
//    thinkDesignRows.foreach(x => res += "<br>" + x)
//    res += "<br>FCM LICENSES"
//    fcmRows.foreach(x => res += "<br>" + x)
    res += "<br>RHINO LICENSES"
    rhinoRows.foreach(x => res += "<br>" + x)
    HttpEntity(ContentTypes.`text/html(UTF-8)`, res)
  }
}
