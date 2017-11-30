package minato.app.config

import minato.app.config.MinatoConfig.{ObjectsMap, RawConfigsMap}
import minato.core.passage._
import minato.core.passage.Spot.{Departure, Port, Terminal}

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

trait MinatoConfig {

  def toRawConfigsMap : RawConfigsMap

  def getObjectMap : ObjectsMap
}

object MinatoConfig{
  case class ConfigId(id: String)

  trait Configs[A] {
    val dependsOn: Seq[ConfigId]

    def toObject(dependencees:ObjectsMap): A

  }

  case class ObjectsMap(departures: Seq[(ConfigId,Departure)], ports: Seq[(ConfigId,Port)], terminals: Seq[(ConfigId,Terminal)],
                        sources: Seq[(ConfigId,Source)], anchors: Seq[(ConfigId,TerminalAnchor)], wareHouses: Seq[(ConfigId,WareHouse)], transShipments: Seq[(ConfigId,TransShipment)],
                        externals: Seq[(ConfigId,ExternalConfig)]){
    def getDeparture(key:ConfigId):Option[Departure] = departures.find(_._1 == key).map(_._2)
    def getPort(key:ConfigId):Option[Port] = ports.find(_._1 == key).map(_._2)
    def getTerminal(key:ConfigId):Option[Terminal] = terminals.find(_._1 == key).map(_._2)
    def getSource(key:ConfigId):Option[Source] = sources.find(_._1 == key).map(_._2)
    def getAnchor(key:ConfigId):Option[TerminalAnchor] = anchors.find(_._1 == key).map(_._2)
    def getWareHouse(key:ConfigId):Option[WareHouse] = wareHouses.find(_._1 == key).map(_._2)
    def getTransShipment(key:ConfigId):Option[TransShipment] = transShipments.find(_._1 == key).map(_._2)
    def getExternalConfigs(key:ConfigId):Option[ExternalConfig] = externals.find(_._1 == key).map(_._2)
  }
  case class RawConfigsMap(departureRawConfigs: Seq[DepartureRawConfig], portRawConfigs: Seq[PortRawConfig], terminalRawConfigs: Seq[TerminalRawConfig],
                           sourceRawConfigs: Seq[SourceRawConfig], anchorRawConfigs: Seq[AnchorRawConfig], wareHouseConfigs: Seq[WareHouseRawConfig], transShipmentConfigs: Seq[TransShipmentRawConfig],
                           externalConfigs: Seq[ExternalRawConfig])

  type DepartureConfig = Configs[Departure]
  type PortConfig = Configs[Port]
  type TerminalConfig = Configs[Terminal]
  type SourceConfig = Configs[Source]
  type AnchorConfig = Configs[TerminalAnchor]
  type WareHouseConfig = Configs[WareHouse]
  type TransShipmentConfig = Configs[TransShipment]
  type ExternalConfig = Configs[ConfigNode]


  trait RawConfig[A] {
    def toConfig: Configs[A]
  }

  type DepartureRawConfig = RawConfig[Departure]
  type PortRawConfig = RawConfig[Port]
  type TerminalRawConfig = RawConfig[Terminal]
  type SourceRawConfig = RawConfig[Source]
  type AnchorRawConfig = RawConfig[TerminalAnchor]
  type WareHouseRawConfig = RawConfig[WareHouse]
  type TransShipmentRawConfig = RawConfig[TransShipment]
  type ExternalRawConfig = RawConfig[ConfigNode]
}

trait MinatoConfigParseException extends Exception{}

object MinatoConfigParseException extends MinatoConfigParseException{

  case object DependenciesCanNotBeResolved  extends MinatoConfigParseException

  case class DuplicateKey(key: String,path:String) extends MinatoConfigParseException
  case class MissingKey(key: String,path:String) extends MinatoConfigParseException
  case class MissingRefId(refId: String,path:String) extends MinatoConfigParseException
  case class MissingAnchorType(anchorType: String,path:String) extends MinatoConfigParseException
  case class MissingSourceType(sourceType: String,path:String) extends MinatoConfigParseException
  case class InvalidRootKey(key: String,path:String) extends MinatoConfigParseException
  case object RootIsNotObject extends MinatoConfigParseException
  case class RootObjectDoesNotHaveIds(key:String,path:String) extends MinatoConfigParseException
  case class RootObjectHasManyIds(key:String,path:String) extends MinatoConfigParseException
  case class ExistsBothOfKeys(key1: String,key2:String, path:String) extends MinatoConfigParseException
  case class IsNotObject(path:String) extends MinatoConfigParseException
  case class IsNotValue(path:String) extends MinatoConfigParseException
  case class IsNotArray(path:String) extends MinatoConfigParseException
  case class OtherException(path:String) extends MinatoConfigParseException
  case class IsNotDestination(refId:String, path:String) extends MinatoConfigParseException
  case class IsNotExternal(refId:String, path:String) extends MinatoConfigParseException

}