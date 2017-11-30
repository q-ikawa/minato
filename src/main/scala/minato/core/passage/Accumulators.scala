package minato.core.passage

import minato.core.cargo.{Cargo, Packet}

/**
  * Created by Daiki_Ikawa on 2017/11/04.
  */
trait Accumulator {
  def apply(packet: Packet):Option[Cargo]
}
