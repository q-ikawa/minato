package minato.app.config

import minato.app.config.ConfigConstants.{ConfigKeysSource}
import minato.app.config.MinatoConfig.{ConfigId}
import minato.app.minatoimpl.sources.StdInJsonSource
import minato.core.passage.Source

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

case class SourceRawConfigImpl(rawConfig: ConfigObject) {

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

  def getSource(configObject: ConfigObject): Source = rawConfig.get(ConfigKeysSource.sourceType) match {
    case Seq(ConfigValue(_, "stdin-json")) => new StdInJsonSource
    case Seq(ConfigValue(_, s)) => throw MinatoConfigParseException.MissingSourceType(s, rawConfig.path)
    case _ => throw MinatoConfigParseException.MissingKey(ConfigKeysSource.sourceType, rawConfig.path)
  }

  def toObject(dependencies: Seq[(ConfigId, Object)]): Source = {
    getSource(getExtConf(dependencies))
  }

}

class SourceConfigs(rawConfig: ConfigObject) extends Configs{
  val sourceRawConfigImpl  = SourceRawConfigImpl(rawConfig)
  val dependsOn: Seq[ConfigId] = sourceRawConfigImpl.getDependant.toSeq
  val confType = ConfType.SourceConf
  val confId = ConfigId(rawConfig.getString(ConfigConstants.ConfigKeys.ID))
  def toObject(dependencies: Seq[(ConfigId,Object)]) : Source ={
    sourceRawConfigImpl.toObject(dependencies)
  }
}