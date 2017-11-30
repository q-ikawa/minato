package minato.app.config

import minato.app.config.ConfigConstants.ConfigKeys
import minato.app.config.MinatoConfig._

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

case class MinatoConfigImpl(configNode: ConfigNode) extends MinatoConfig {
  override def getObjectMap = {

    ???

  }

  def getRawConfigsObject(configNode: ConfigNode): Seq[ConfigObject] = configNode match {
    case configArray: ConfigArray => configArray.values.flatMap(getRawConfigsObject)
    case configObject: ConfigObject => {
      configObject.get(ConfigKeys.ID) match {
        case Seq(ConfigValue(_, _)) => Seq(configObject)
        case _ :: _ :: _ => throw MinatoConfigParseException.RootObjectHasManyIds(configObject.path, ConfigKeys.ID)
        case Nil => throw MinatoConfigParseException.RootObjectDoesNotHaveIds(configObject.path, ConfigKeys.ID)
      }
    }
    case _ => throw MinatoConfigParseException.RootIsNotObject
  }

  override def toRawConfigsMap: RawConfigsMap = {
    configNode match {
      case configObject: ConfigObject =>
        val departures = configObject.get(ConfigConstants.ConfigKeys.Departures).flatMap(getRawConfigsObject).map(DepartureRawConfigImpl)
        val ports = configObject.get(ConfigConstants.ConfigKeys.Ports).flatMap(getRawConfigsObject).map(PortRawConfigImpl)
        val terminals = configObject.get(ConfigConstants.ConfigKeys.Terminals).flatMap(getRawConfigsObject).map(TerminalRawConfigImpl)
        val sources = configObject.get(ConfigConstants.ConfigKeys.Sources).flatMap(getRawConfigsObject).map(SourceRawConfigImpl)
        val anchors = configObject.get(ConfigConstants.ConfigKeys.Anchors).flatMap(getRawConfigsObject).map(AnchorRawConfigImpl)
        val warehouses = configObject.get(ConfigConstants.ConfigKeys.Warehouses).flatMap(getRawConfigsObject).map(WarehouseRawConfigImpl)
        val transShipments = configObject.get(ConfigConstants.ConfigKeys.TransShipments).flatMap(getRawConfigsObject).map(TransShipmentRawConfigImpl)
        val externals = configObject.get(ConfigConstants.ConfigKeys.Externals).flatMap(getRawConfigsObject).map(ExternalRawConfigImpl)
        RawConfigsMap(departures, ports, terminals, sources, anchors, warehouses, transShipments, externals)
      case _ => throw MinatoConfigParseException.RootIsNotObject
    }
  }


}


case class WarehouseRawConfigImpl(rawConfig: ConfigObject) extends WareHouseRawConfig {
  override def toConfig = ???
}

case class TransShipmentRawConfigImpl(rawConfig: ConfigObject) extends TransShipmentRawConfig {
  override def toConfig = ???
}

case class ExternalRawConfigImpl(rawConfig: ConfigObject) extends ExternalRawConfig {
  override def toConfig = ???
}


