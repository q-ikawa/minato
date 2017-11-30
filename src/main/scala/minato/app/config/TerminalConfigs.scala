package minato.app.config

import minato.app.config.ConfigConstants.ConfigKeysTerminal
import minato.app.config.MinatoConfig.{ConfigId, ObjectsMap, TerminalConfig, TerminalRawConfig}
import minato.core.passage.Spot.Terminal
import minato.core.passage.{Source, SpotId, TerminalAnchor}

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

case class TerminalRawConfigImpl(rawConfig: ConfigObject) {
  val refIdSeq = rawConfig.get(ConfigKeysTerminal.refId)
  val configSeq = rawConfig.get(ConfigKeysTerminal.config)

  val toConfig = (refIdSeq, configSeq) match {
    case (Nil, Nil) => throw MinatoConfigParseException.MissingKey(ConfigKeysTerminal.config, rawConfig.path)
    case (_ :: _, _ :: _) => throw MinatoConfigParseException.ExistsBothOfKeys(ConfigKeysTerminal.refId, ConfigKeysTerminal.config, rawConfig.path)
    case (_ :: _ :: _, _) => throw MinatoConfigParseException.DuplicateKey(ConfigKeysTerminal.refId, rawConfig.path)
    case (_, _ :: _ :: _) => throw MinatoConfigParseException.DuplicateKey(ConfigKeysTerminal.config, rawConfig.path)
    case (ConfigValue(_, refId) :: Nil, Nil) => new TerminalConfigFromRefId(rawConfig, refId)
    case (Nil, (nestedConfigObject: ConfigObject) :: Nil) => new TerminalConfigFromNestedConfig(nestedConfigObject, rawConfig)
    case _ => throw MinatoConfigParseException.OtherException(rawConfig.path)
  }
}

class TerminalConfigFromRefId(rawConfig: ConfigObject, refId: String) extends Configs {
  override def toObject(dependencies: Seq[(ConfigId,Object)]) = {
    val RefId = ConfigId(refId)
    val anchor = dependencies.find({case (RefId,t:TerminalAnchor) => true}).map(_._2.asInstanceOf[TerminalAnchor]) match {
      case None => throw MinatoConfigParseException.MissingRefId(refId, rawConfig.path)
      case Some(s) => s
    }
    Terminal(SpotId(rawConfig.key), anchor)
  }

  val confType = ConfType.TerminalConf
  val confId =ConfigId( rawConfig.getString(ConfigConstants.ConfigKeys.ID))
  override val dependsOn: Seq[ConfigId] = Seq(ConfigId(refId))
}

class TerminalConfigFromNestedConfig(nestedConfigObject: ConfigObject, parentConfig: ConfigObject) extends Configs {
  override def toObject(dependencies: Seq[(ConfigId,Object)]) = {
    val anchor: TerminalAnchor = AnchorRawConfigImpl(nestedConfigObject).toObject(dependencies)
    Terminal(SpotId(parentConfig.key), anchor)
  }

  val confType = ConfType.TerminalConf
  val confId = ConfigId(parentConfig.getString(ConfigConstants.ConfigKeys.ID))
  override val dependsOn: Seq[ConfigId] = Nil
}
