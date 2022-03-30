package deepsea.database

import deepsea.materials.MaterialManager.{Material, MaterialNode}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._

trait MongoCodecs {

  implicit val MaterialDecoder: Decoder[Material] = deriveDecoder[Material]
  implicit val MaterialEncoder: Encoder[Material] = deriveEncoder[Material]

  implicit val MaterialNodeDecoder: Decoder[MaterialNode] = deriveDecoder[MaterialNode]
  implicit val MaterialNodeEncoder: Encoder[MaterialNode] = deriveEncoder[MaterialNode]

  def codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[Material],
    classOf[MaterialNode],
  ), DEFAULT_CODEC_REGISTRY)
}
