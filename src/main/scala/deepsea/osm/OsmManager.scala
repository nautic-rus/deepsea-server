package deepsea.osm

import akka.actor.Actor
import deepsea.dbase.{DatabaseManager, MongoCodecs}
import deepsea.osm.OsmManager.{AddPLS, GetPLS, ParkingLocationSheet}
import deepsea.time.TimeControlManager.SpyWatch
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal}
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object OsmManager{
  case class OSMUser(id: String, name: String, surname: String)
  case class LatLng(lat: Double, lng: Double)
  case class ParkingLocationSheet(user: OSMUser, points: List[LatLng], address: String, date: Long, time: Long, comment: String, status: String)

  case class AddPLS(json: String)
  case class GetPLS()
}
class OsmManager extends Actor with MongoCodecs{
  val pls: ListBuffer[ParkingLocationSheet] = ListBuffer.empty[ParkingLocationSheet]
  override def receive: Receive = {
    case AddPLS(json) =>
      decode[ParkingLocationSheet](json) match {
        case Right(value) =>
          pls += value
        case Left(value) =>
      }
      sender() ! "success".asJson.noSpaces
    case GetPLS() =>
      sender() ! pls.asJson.noSpaces
    case _ => None
  }
}
