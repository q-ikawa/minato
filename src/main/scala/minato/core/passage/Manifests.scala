package minato.core.passage

import minato.core.cargo._
import minato.core.passage.Spot.Destination
import minato.core.types.OneOrMore

/**
  * Created by Daiki_Ikawa on 2017/11/04.
  */


case class Manifest(transShipment: Option[TransShipment], manifestItem: ManifestItem) {
  lazy val destinations: Seq[Destination] = for (consignee <- manifestItem.consignees; destination <- consignee.destinations) yield destination
  private val composedTransshippment = transShipment.getOrElse(TransShipment.Identity)

  val preparedWareHouse = manifestItem.wareHouse receiveInstruction { packetSeq =>
    for (consignee <- manifestItem.consignees;
         destination <- consignee.destinations;
         cargo <- consignee.loader load packetSeq)
      cargo >>: destination
  }

  def apply(cargoInput: Cargo): Unit = {
    for (cargo <- cargoInput >>: composedTransshippment;
         packetSeq <- manifestItem.unloader unload cargo)
      packetSeq >>: preparedWareHouse
  }
}

case class ManifestItem(unloader: Unloader, wareHouse: WareHouse, consignees: OneOrMore[Consignee])

case class Consignee(loader: Loader, destinations: OneOrMore[Destination])

case class Unloader(preFilter: Option[Filter], unpackers: Seq[Unpacker], decorators: Seq[Decorator]) {
  def unload(cargo: Cargo): Option[Seq[Packet]] = {
    val composedUnpackers: Packet => Seq[Packet] = packet => unpackers match {
      case Nil => Seq(packet)
      case _ :: _ => unpackers.foldLeft(Nil: Seq[Packet])((seq, unpacker) => (seq ++ unpacker(packet)))
    }
    val composedUnpackersWithSource: Packet => Seq[(Packet, Packet)] = source => for (pack <- composedUnpackers(source)) yield (pack, source)
    val composedDecorators: (Packet, Packet) => Packet = (packet, source) => decorators match {
      case Nil => packet
      case _ :: _ => decorators.foldLeft(packet) { (p, decorator) => decorator(p, source) }
    }
    cargo.getPacket.filter(preFilter.getOrElse(NoFilters)(_))
      .map(composedUnpackersWithSource)
      .map(seq => seq.map(sourceAndPackage => composedDecorators(sourceAndPackage._1, sourceAndPackage._2)))
  }

}

case class Loader(filter: Option[Filter], accumulator: Accumulator) {
  def load(packets: Seq[Packet]): Option[Cargo] = accumulator(Packet(packets.filter(filter.getOrElse(NoFilters)(_))))
}

case class Decorator(to: Label, from: Label) {
  def apply(target: Packet, source: Packet): Packet = {
    source.get(from).foldLeft(target) { (target, importPacket) => target + (to, importPacket) }
  }
}

case class Unpacker(label: Label) {
  def apply(packet: Packet): Seq[Packet] = packet.get(label)
}

trait WareHouse {
  type WareHouseInstruction = Seq[Packet] => Unit
  def receiveInstruction(instruction: WareHouseInstruction): PreparedWareHouse
}

trait PreparedWareHouse {
  def >>:(packet: Seq[Packet]): Unit
}

trait TransShipment {
  def >>:(cargo: Cargo): Option[Cargo]
}

object TransShipment {
  val Identity: TransShipment = cargo => Some(cargo)
}