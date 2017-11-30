package minato.app.config

import minato.app.config.ConfType.DepartureConf
import minato.app.config.ConfigConstants.ConfigKeysDeparture
import minato.app.config.MinatoConfig._
import minato.core.passage.Spot.{Departure, Destination}
import minato.core.passage.{Source, Spot, SpotId}
import minato.core.types.OneOrMore

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

case class DepartureRawConfigImpl(rawConfig: ConfigObject) {
  def toConfig: Configs = {
    val refIdSeq = rawConfig.get(ConfigKeysDeparture.refId)
    val configSeq = rawConfig.get(ConfigKeysDeparture.config)

    val destinations:Seq[ConfigId] = rawConfig.get(ConfigKeysDeparture.destinations) match {
      case Seq(configNode) => configNode match {
        case ConfigArray(_,values: Seq[ConfigNode]) => values.map{
          case ConfigValue(_,s) => ConfigId(s)
          case _ => throw MinatoConfigParseException.IsNotValue(configNode.path)
        }
        case ConfigValue(_, s) => Seq(ConfigId(s))
        case _ => throw MinatoConfigParseException.IsNotArray(configNode.path)
      }
      case Nil => throw MinatoConfigParseException.MissingKey(ConfigKeysDeparture.destinations,rawConfig.path)
      case _::_::_ => throw MinatoConfigParseException.DuplicateKey(ConfigKeysDeparture.destinations,rawConfig.path)
    }

    (refIdSeq, configSeq) match {
      case (Nil, Nil) => throw MinatoConfigParseException.MissingKey(ConfigKeysDeparture.config, rawConfig.path)
      case (_ :: _, _ :: _) => throw MinatoConfigParseException.ExistsBothOfKeys(ConfigKeysDeparture.refId, ConfigKeysDeparture.config, rawConfig.path)
      case (_ :: _ :: _, _) => throw MinatoConfigParseException.DuplicateKey(ConfigKeysDeparture.refId, rawConfig.path)
      case (_, _ :: _ :: _) => throw MinatoConfigParseException.DuplicateKey(ConfigKeysDeparture.config, rawConfig.path)
      case (ConfigValue(_, refId) :: Nil, Nil) => new DepartureConfigFromRefId(rawConfig, refId , destinations)
      case (Nil, (nestedConfigObject: ConfigObject) :: Nil) => new DepartureConfigFromNestedConfig(nestedConfigObject, rawConfig, destinations)
      case _ => throw MinatoConfigParseException.OtherException(rawConfig.path)
    }
  }
}

class DepartureConfigFromRefId(rawConfig: ConfigObject, refId: String, destinations:Seq[ConfigId]) extends Configs {
  val confType:ConfType = DepartureConf
  val confId = ConfigId(rawConfig.getString(ConfigConstants.ConfigKeys.ID))

  override def toObject(dependencies: Seq[(ConfigId,Object)]) = {
    val RefId = ConfigId(refId)
    val source = dependencies.find({case (RefId,s:Source) => true}).map(_._2.asInstanceOf[Source]) match {
      case None => throw MinatoConfigParseException.MissingRefId(refId, rawConfig.path)
      case Some(s) => s
    }
    val dest:Seq[Destination] = destinations.map(
      id => {dependencies.find(o => o._1 == id) match{
        case Some((_,s : Destination)) => s
        case Some(_) => throw MinatoConfigParseException.IsNotDestination(id.id,rawConfig.path)
        case None => throw MinatoConfigParseException.MissingRefId(id.id,rawConfig.path)
      }
    })
    Departure(SpotId(rawConfig.key),source,OneOrMore(dest))
  }

  override val dependsOn: Seq[ConfigId] = Seq(ConfigId(refId)) ++ destinations
}

class DepartureConfigFromNestedConfig(nestedConfigObject: ConfigObject, parentConfig: ConfigObject,destinations:Seq[ConfigId]) extends Configs {
  val confType:ConfType = DepartureConf
  val confId = ConfigId(parentConfig.getString(ConfigConstants.ConfigKeys.ID))

  override def toObject(dependencies: Seq[(ConfigId,Object)]) = {
    val source: Source = SourceRawConfigImpl(nestedConfigObject).toObject(dependencies)

    val dest:Seq[Destination] = destinations.map(
      id => {dependencies.find(o => o._1 == id) match{
        case Some((_,s : Destination)) => s
        case Some(_) => throw MinatoConfigParseException.IsNotDestination(id.id,parentConfig.path)
        case None => throw MinatoConfigParseException.MissingRefId(id.id,parentConfig.path)
      }
      })
    Departure(SpotId(parentConfig.key),source,OneOrMore(dest))
  }

  override val dependsOn: Seq[ConfigId] = destinations
}
