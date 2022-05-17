package deepsea.database

import deepsea.files.FileManager.{TreeDirectory, TreeFile, TreeFileHistory}
import deepsea.materials.MaterialManager.{Material, MaterialHistory, MaterialNode, WCNumberName, WeightControl}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._

trait MongoCodecs {

  implicit val MaterialDecoder: Decoder[Material] = deriveDecoder[Material]
  implicit val MaterialEncoder: Encoder[Material] = deriveEncoder[Material]
  
  implicit val MaterialHistoryDecoder: Decoder[MaterialHistory] = deriveDecoder[MaterialHistory]
  implicit val MaterialHistoryEncoder: Encoder[MaterialHistory] = deriveEncoder[MaterialHistory]

  implicit val MaterialNodeDecoder: Decoder[MaterialNode] = deriveDecoder[MaterialNode]
  implicit val MaterialNodeEncoder: Encoder[MaterialNode] = deriveEncoder[MaterialNode]

  implicit val TreeFileDecoder: Decoder[TreeFile] = deriveDecoder[TreeFile]
  implicit val TreeFileEncoder: Encoder[TreeFile] = deriveEncoder[TreeFile]

  implicit val TreeDirectoryDecoder: Decoder[TreeDirectory] = deriveDecoder[TreeDirectory]
  implicit val TreeDirectoryEncoder: Encoder[TreeDirectory] = deriveEncoder[TreeDirectory]

  implicit val TreeFileHistoryDecoder: Decoder[TreeFileHistory] = deriveDecoder[TreeFileHistory]
  implicit val TreeFileHistoryEncoder: Encoder[TreeFileHistory] = deriveEncoder[TreeFileHistory]

  implicit val WeightControlDecoder: Decoder[WeightControl] = deriveDecoder[WeightControl]
  implicit val WeightControlEncoder: Encoder[WeightControl] = deriveEncoder[WeightControl]

  implicit val WCNumberNameDecoder: Decoder[WCNumberName] = deriveDecoder[WCNumberName]
  implicit val WCNumberNameEncoder: Encoder[WCNumberName] = deriveEncoder[WCNumberName]


  def codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[Material],
    classOf[MaterialHistory],
    classOf[MaterialNode],
    classOf[TreeFile],
    classOf[TreeDirectory],
    classOf[TreeFileHistory],
    classOf[WeightControl],
    classOf[WCNumberName],
  ), DEFAULT_CODEC_REGISTRY)
}
