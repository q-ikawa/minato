package minato.app.config

import minato.algorithms.{DirectedAcyclicGraph, Edge, Vertex}
import minato.app.config.ConfType._
import minato.app.config.MinatoConfig.ConfigId
import minato.core.passage.{Itinerary, Spot}

/**
  * Created by Daiki_Ikawa on 2017/11/28.
  */

trait Configs {
  val confType: ConfType
  val confId: ConfigId
  val dependsOn: Seq[ConfigId]

  def toObject(dependencies: Seq[(ConfigId, Object)]): Object
}

sealed trait ConfType

object ConfType {
  def apply(s: String): ConfType = s match {
    case ConfigConstants.ConfigKeys.Departures => DepartureConf
    case ConfigConstants.ConfigKeys.Ports => PortConf
    case ConfigConstants.ConfigKeys.Terminals => TerminalConf
    case ConfigConstants.ConfigKeys.Anchors => AnchorConf
    case ConfigConstants.ConfigKeys.Sources => SourceConf
    case ConfigConstants.ConfigKeys.Warehouses => WareHouseConf
    case ConfigConstants.ConfigKeys.TransShipments => TransShipmentConf
    case ConfigConstants.ConfigKeys.Externals => ExternalConf
  }

  case object DepartureConf extends ConfType

  case object PortConf extends ConfType

  case object TerminalConf extends ConfType

  case object AnchorConf extends ConfType

  case object SourceConf extends ConfType

  case object WareHouseConf extends ConfType

  case object TransShipmentConf extends ConfType

  case object ExternalConf extends ConfType

}


object MinatoConfigLoader {
  def load(configNode: ConfigNode): Either[Exception, Itinerary] = {
    try {
      sortConfigs(rootNodeToSeq(configNode)) match {
        case None => Left(MinatoConfigParseException.DependenciesCanNotBeResolved)
        case Some(seq) => {
          val spots = seq.foldLeft(Nil: Seq[(ConfigId, Object)])(getNextDependencies).collect { case (_, s: Spot) => s }
          Right(Itinerary(spots))
        }
      }
    } catch {
      case e: Exception => Left(e)
    }
  }

  def rootNodeToSeq(configNode: ConfigNode): Seq[Configs] = configNode match {
    case c: ConfigObject => c.entrySet.map(x =>
      x._2 match {
        case obj: ConfigObject => {
          ConfType(x._1) match {
            case DepartureConf => DepartureRawConfigImpl(obj).toConfig
            case SourceConf => new SourceConfigs(obj)
            case AnchorConf => new AnchorConfigs(obj)
            case PortConf => PortRawConfigImpl(obj).toConfig
            case TerminalConf => TerminalRawConfigImpl(obj).toConfig
            case WareHouseConf => ???
            case ExternalConf => ???
            case TransShipmentConf => ???
          }
        }
        case other => throw MinatoConfigParseException.IsNotObject(other.path)
      }
    )
    case _ => throw MinatoConfigParseException.RootIsNotObject
  }


  def sortConfigs(seq: Seq[Configs]): Option[Seq[Configs]] = {
    val confIdToConf = seq.map(conf => conf.confId -> conf).toMap
    val vertexSet = seq.map(_.confId).map(Vertex(_)).toSet
    val edge = seq.flatMap(conf => conf.dependsOn.map(dependsOn => Edge(Vertex(dependsOn),Vertex(conf.confId)))).toSet
    DirectedAcyclicGraph(vertexSet, edge).tsort.map(_.map(confIdToConf))
  }

  def getNextDependencies(dependencies: Seq[(ConfigId, Object)], conf: Configs): Seq[(ConfigId, Object)] = Seq(conf.confId -> conf.toObject(dependencies)) ++ dependencies

}