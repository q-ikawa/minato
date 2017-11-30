package minato.app.config

import minato.app.config.ConfigConstants.{ConfigKeysAnchor, ConfigKeysSource}
import minato.app.config.MinatoConfig.{AnchorConfig, AnchorRawConfig, ConfigId, ObjectsMap}
import minato.app.config.MinatoConfigParseException.MissingRefId
import minato.app.minatoimpl.terminalAnchors.{StdErrAnchor, StdOutAnchor}
import minato.core.passage.{Source, TerminalAnchor}

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

case class AnchorRawConfigImpl(rawConfig: ConfigObject) {
  lazy val getDependant = rawConfig.get(ConfigKeysSource.extRef) match {
    case Seq(ConfigValue(_, extRef)) => Some(ConfigId(extRef))
    case Nil => None
    case _ :: _ => throw MinatoConfigParseException.DuplicateKey(ConfigKeysSource.extRef, rawConfig.path)
  }

  def getExtConf(objects: Seq[(ConfigId, Object)]): ConfigObject = getDependant match {
    case Some(extRef) => objects.find({ case (refId, s: Source) if refId == extRef => true }).map(_._2.asInstanceOf[ConfigObject]) match {
      case Some(co: ConfigObject) => co
      case Some(_) => throw MinatoConfigParseException.IsNotExternal(extRef.id, rawConfig.path)
      case None => throw MinatoConfigParseException.MissingRefId(extRef.id, rawConfig.path)
    }
    case None => rawConfig
  }

  def getAnchor(configObject: ConfigObject): TerminalAnchor = rawConfig.get(ConfigKeysAnchor.anchorType) match {
    case Seq(ConfigValue(_, "stdout")) => new StdOutAnchor
    case Seq(ConfigValue(_, "stderr")) => new StdErrAnchor
    case Seq(ConfigValue(_, s)) => throw MinatoConfigParseException.MissingAnchorType(s, rawConfig.path)
    case _ => throw MinatoConfigParseException.MissingKey(ConfigKeysAnchor.anchorType, rawConfig.path)
  }

  def toObject(dependencees: Seq[(ConfigId, Object)]): TerminalAnchor = {
    getAnchor(getExtConf(dependencees))
  }

  val dependsOn: Seq[ConfigId] = getDependant.toSeq

}


class AnchorConfigs(rawConfig: ConfigObject) extends Configs {
  val anchorRawConfigImpl = AnchorRawConfigImpl(rawConfig)
  val dependsOn: Seq[ConfigId] = anchorRawConfigImpl.getDependant.toSeq
  val confType = ConfType.SourceConf
  val confId = ConfigId(rawConfig.getString(ConfigConstants.ConfigKeys.ID))

  def toObject(dependencies: Seq[(ConfigId, Object)]): TerminalAnchor = {
    anchorRawConfigImpl.toObject(dependencies)
  }
}