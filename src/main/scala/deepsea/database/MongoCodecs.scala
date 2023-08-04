package deepsea.database

import deepsea.auth.AuthManager.{AdminRight, Department, Page, RightUser, Role, User, UserNotification, UserProject}
import deepsea.fest.FestManager.{FestKaraoke, FestSauna, FestStories}
import deepsea.fest.classes.{BestPlayer, Mark, TeamWon}
import deepsea.files.FileManager.{DocumentDirectories, TreeDirectory, TreeFile, TreeFileHistory}
import deepsea.issues.IssueManager.{DailyTask, DayCalendar, IdName, IssueDef, IssueProject, LV, MessageReaction}
import deepsea.materials.MaterialManager.{Material, MaterialHistory, MaterialNode, MaterialNodeHistory, MaterialTranslation, ProjectName, WCNumberName, WeightControl}
import deepsea.mobile.MobileManager.{Drawing, DrawingInfo, OrizInfo}
import deepsea.osm.OsmManager.{LatLng, OSMUser, ParkingLocationSheet}
import deepsea.time.PlanHoursManager.{ConsumedHour, PlanHour, PlannedHours}
import deepsea.time.PlanManager.{DayInterval, IssuePlan, PlanByDays, PlanInterval, UserPlan}
import deepsea.time.TimeControlManager.{SpyWatch, TimeControlInterval, UserWatch}
import io.circe.generic.semiauto.{deriveCodec, deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros
import org.mongodb.scala.bson.codecs.Macros._

import java.util.Date

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

  implicit val OrizInfoNameDecoder: Decoder[OrizInfo] = deriveDecoder[OrizInfo]
  implicit val OrizInfoNameEncoder: Encoder[OrizInfo] = deriveEncoder[OrizInfo]

  implicit val DrawingDecoder: Decoder[Drawing] = deriveDecoder[Drawing]
  implicit val DrawingEncoder: Encoder[Drawing] = deriveEncoder[Drawing]


  implicit val MaterialNodeHistoryDecoder: Decoder[MaterialNodeHistory] = deriveDecoder[MaterialNodeHistory]
  implicit val MaterialNodeHistoryEncoder: Encoder[MaterialNodeHistory] = deriveEncoder[MaterialNodeHistory]

  implicit val MessageReactionDecoder: Decoder[MessageReaction] = deriveDecoder[MessageReaction]
  implicit val MessageReactionEncoder: Encoder[MessageReaction] = deriveEncoder[MessageReaction]

  implicit val DrawingInfoDecoder: Decoder[DrawingInfo] = deriveDecoder[DrawingInfo]
  implicit val DrawingInfoEncoder: Encoder[DrawingInfo] = deriveEncoder[DrawingInfo]

  implicit val UserWatchDecoder: Decoder[UserWatch] = deriveDecoder[UserWatch]
  implicit val UserWatchEncoder: Encoder[UserWatch] = deriveEncoder[UserWatch]

  implicit val MaterialTranslationDecoder: Decoder[MaterialTranslation] = deriveDecoder[MaterialTranslation]
  implicit val MaterialTranslationEncoder: Encoder[MaterialTranslation] = deriveEncoder[MaterialTranslation]

  implicit val ProjectNameDecoder: Decoder[ProjectName] = deriveDecoder[ProjectName]
  implicit val ProjectNameEncoder: Encoder[ProjectName] = deriveEncoder[ProjectName]

  implicit val DocumentDirectoriesDecoder: Decoder[DocumentDirectories] = deriveDecoder[DocumentDirectories]
  implicit val DocumentDirectoriesEncoder: Encoder[DocumentDirectories] = deriveEncoder[DocumentDirectories]

  implicit val DailyTaskDecoder: Decoder[DailyTask] = deriveDecoder[DailyTask]
  implicit val DailyTaskEncoder: Encoder[DailyTask] = deriveEncoder[DailyTask]

  implicit val SpyWatchDecoder: Decoder[SpyWatch] = deriveDecoder[SpyWatch]
  implicit val SpyWatchEncoder: Encoder[SpyWatch] = deriveEncoder[SpyWatch]

  implicit val ParkingLocationSheetDecoder: Decoder[ParkingLocationSheet] = deriveDecoder[ParkingLocationSheet]
  implicit val ParkingLocationSheetEncoder: Encoder[ParkingLocationSheet] = deriveEncoder[ParkingLocationSheet]

  implicit val OSMUserDecoder: Decoder[OSMUser] = deriveDecoder[OSMUser]
  implicit val OSMUserEncoder: Encoder[OSMUser] = deriveEncoder[OSMUser]

  implicit val LatLngDecoder: Decoder[LatLng] = deriveDecoder[LatLng]
  implicit val LatLngEncoder: Encoder[LatLng] = deriveEncoder[LatLng]

  implicit val IssueProjectDecoder: Decoder[IssueProject] = deriveDecoder[IssueProject]
  implicit val IssueProjectEncoder: Encoder[IssueProject] = deriveEncoder[IssueProject]

  implicit val RoleDecoder: Decoder[Role] = deriveDecoder[Role]
  implicit val RoleEncoder: Encoder[Role] = deriveEncoder[Role]

  implicit val UserDecoder: Decoder[User] = deriveDecoder[User]
  implicit val UserEncoder: Encoder[User] = deriveEncoder[User]

  implicit val PageDecoder: Decoder[Page] = deriveDecoder[Page]
  implicit val PageEncoder: Encoder[Page] = deriveEncoder[Page]

  implicit val RightUserDecoder: Decoder[RightUser] = deriveDecoder[RightUser]
  implicit val RightUserEncoder: Encoder[RightUser] = deriveEncoder[RightUser]

  implicit val AdminRightDecoder: Decoder[AdminRight] = deriveDecoder[AdminRight]
  implicit val AdminRightEncoder: Encoder[AdminRight] = deriveEncoder[AdminRight]

  implicit val PlanHourDecoder: Decoder[PlanHour] = deriveDecoder[PlanHour]
  implicit val PlanHourEncoder: Encoder[PlanHour] = deriveEncoder[PlanHour]

  implicit val DepartmentDecoder: Decoder[Department] = deriveDecoder[Department]
  implicit val DepartmentEncoder: Encoder[Department] = deriveEncoder[Department]

  implicit val IdNameDecoder: Decoder[IdName] = deriveDecoder[IdName]
  implicit val IdNameEncoder: Encoder[IdName] = deriveEncoder[IdName]

  implicit val LVDecoder: Decoder[LV] = deriveDecoder[LV]
  implicit val LVEncoder: Encoder[LV] = deriveEncoder[LV]

  implicit val DayCalendarDecoder: Decoder[DayCalendar] = deriveDecoder[DayCalendar]
  implicit val DayCalendarEncoder: Encoder[DayCalendar] = deriveEncoder[DayCalendar]

  implicit val IssueDefDecoder: Decoder[IssueDef] = deriveDecoder[IssueDef]
  implicit val IssueDefEncoder: Encoder[IssueDef] = deriveEncoder[IssueDef]

  implicit val TimeControlIntervalDecoder: Decoder[TimeControlInterval] = deriveDecoder[TimeControlInterval]
  implicit val TimeControlIntervalEncoder: Encoder[TimeControlInterval] = deriveEncoder[TimeControlInterval]

  implicit val FestStoriesDecoder: Decoder[FestStories] = deriveDecoder[FestStories]
  implicit val FestStoriesEncoder: Encoder[FestStories] = deriveEncoder[FestStories]

  implicit val FestKaraokeDecoder: Decoder[FestKaraoke] = deriveDecoder[FestKaraoke]
  implicit val FestKaraokeEncoder: Encoder[FestKaraoke] = deriveEncoder[FestKaraoke]

  implicit val FestSaunaDecoder: Decoder[FestSauna] = deriveDecoder[FestSauna]
  implicit val FestSaunaEncoder: Encoder[FestSauna] = deriveEncoder[FestSauna]

  implicit val MarkDecoder: Decoder[Mark] = deriveDecoder[Mark]
  implicit val MarkEncoder: Encoder[Mark] = deriveEncoder[Mark]

  implicit val TeamWonDecoder: Decoder[TeamWon] = deriveDecoder[TeamWon]
  implicit val TeamWonEncoder: Encoder[TeamWon] = deriveEncoder[TeamWon]

  implicit val BestPlayerDecoder: Decoder[BestPlayer] = deriveDecoder[BestPlayer]
  implicit val BestPlayerEncoder: Encoder[BestPlayer] = deriveEncoder[BestPlayer]

  implicit val PlannedHoursDecoder: Decoder[PlannedHours] = deriveDecoder[PlannedHours]
  implicit val PlannedHoursEncoder: Encoder[PlannedHours] = deriveEncoder[PlannedHours]

  implicit val ConsumedHourDecoder: Decoder[ConsumedHour] = deriveDecoder[ConsumedHour]
  implicit val ConsumedHourEncoder: Encoder[ConsumedHour] = deriveEncoder[ConsumedHour]

  implicit val PlanIntervalDecoder: Decoder[PlanInterval] = deriveDecoder[PlanInterval]
  implicit val PlanIntervalEncoder: Encoder[PlanInterval] = deriveEncoder[PlanInterval]

  implicit val DayIntervalDecoder: Decoder[DayInterval] = deriveDecoder[DayInterval]
  implicit val DayIntervalEncoder: Encoder[DayInterval] = deriveEncoder[DayInterval]

  implicit val PlanByDaysDecoder: Decoder[PlanByDays] = deriveDecoder[PlanByDays]
  implicit val PlanByDaysEncoder: Encoder[PlanByDays] = deriveEncoder[PlanByDays]

  implicit val UserPlanDecoder: Decoder[UserPlan] = deriveDecoder[UserPlan]
  implicit val UserPlanEncoder: Encoder[UserPlan] = deriveEncoder[UserPlan]

  implicit val IssuePlanDecoder: Decoder[IssuePlan] = deriveDecoder[IssuePlan]
  implicit val IssuePlanEncoder: Encoder[IssuePlan] = deriveEncoder[IssuePlan]

  implicit val UserNotificationDecoder: Decoder[UserNotification] = deriveDecoder[UserNotification]
  implicit val UserNotificationEncoder: Encoder[UserNotification] = deriveEncoder[UserNotification]

  implicit val UserProjectDecoder: Decoder[UserProject] = deriveDecoder[UserProject]
  implicit val UserProjectEncoder: Encoder[UserProject] = deriveEncoder[UserProject]





  def codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    Macros.createCodecProviderIgnoreNone[Material],
    classOf[MaterialHistory],
    classOf[MaterialNode],
    classOf[TreeFile],
    classOf[TreeDirectory],
    classOf[TreeFileHistory],
    classOf[WeightControl],
    classOf[WCNumberName],
    classOf[Drawing],
    classOf[DrawingInfo],
    classOf[MaterialNodeHistory],
    classOf[UserWatch],
    classOf[MaterialTranslation],
    classOf[ProjectName],
    classOf[DocumentDirectories],
    classOf[DailyTask],
    classOf[SpyWatch],
    classOf[ParkingLocationSheet],
    classOf[IssueProject],
    classOf[Role],
    classOf[User],
    classOf[AdminRight],
    classOf[PlanHour],
    classOf[Department],
    classOf[FestStories],
    classOf[FestKaraoke],
    classOf[FestSauna],
    classOf[Mark],
    classOf[TeamWon],
    classOf[ConsumedHour],
    classOf[PlanInterval],
    classOf[DayInterval],
    classOf[PlanByDays],
    classOf[UserPlan],
    classOf[IssuePlan],
  ), DEFAULT_CODEC_REGISTRY)
}
