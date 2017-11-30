package minato.app.config

import minato.app.config.ConfType.{DepartureConf, PortConf}
import minato.app.config.ConfigConstants.{ConfigKeysDeparture, ConfigKeysPort}
import minato.app.config.MinatoConfig.{ConfigId, ObjectsMap, PortConfig, PortRawConfig}
import minato.app.minatoimpl.accumulators.MustacheAccumulator
import minato.app.minatoimpl.warehouses.ByPassWareHouse
import minato.core.passage.{Accumulator, Consignee, Loader, Manifest, ManifestItem, Source, SpotId, TransShipment, Unloader, WareHouse}
import minato.core.passage.Spot.{Departure, Destination, Port}
import minato.core.types.OneOrMore

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */

case class PortRawConfigImpl(rawConfig: ConfigObject) {

  def toConfig: Configs = rawConfig match {
    case c if c.contains(ConfigKeysPort.destinations) => new FlatPortConfigImpl(rawConfig)
    case _ => throw MinatoConfigParseException.OtherException("")
  }
}

class FlatPortConfigImpl(rawConfig: ConfigObject) extends Configs {
  val confType: ConfType = PortConf
  val confId = ConfigId(rawConfig.getString(ConfigConstants.ConfigKeys.ID))

  val destinations: Seq[ConfigId] = rawConfig.get(ConfigKeysPort.destinations) match {
    case Seq(configNode) => configNode match {
      case ConfigArray(_, values: Seq[ConfigNode]) => values.map {
        case ConfigValue(_, s) => ConfigId(s)
        case c: ConfigNode => throw MinatoConfigParseException.IsNotValue(c.path)
      }
      case ConfigValue(_, s) => Seq(ConfigId(s))
      case _ => throw MinatoConfigParseException.IsNotArray(configNode.path)
    }
    case Nil => throw MinatoConfigParseException.MissingKey(ConfigKeysPort.destinations, rawConfig.path)
    case _ :: _ :: _ => throw MinatoConfigParseException.DuplicateKey(ConfigKeysPort.destinations, rawConfig.path)
  }

  def toObject(dependencies: Seq[(ConfigId, Object)]) = {

    val accumulator: Accumulator = new MustacheAccumulator(rawConfig.getString("mustache"))
    val unloader: Unloader = Unloader(None, Nil, Nil)
    val warehouse: WareHouse = new ByPassWareHouse
    val loader: Loader = Loader(None, accumulator)

    val consignee: Consignee = Consignee(loader, OneOrMore(destinations.flatMap(id => {
      dependencies.collect({ case (ii, d: Destination) if ii == id => d })
    })))
    val manifestItem: ManifestItem = ManifestItem(unloader, warehouse, OneOrMore(Seq(consignee)))
    val transShipment: Option[TransShipment] = None
    val manifest: Manifest = Manifest(transShipment, manifestItem)
    Port(SpotId(rawConfig.key), OneOrMore(Seq(manifest)))
  }

  override val dependsOn: Seq[ConfigId] = destinations

}
