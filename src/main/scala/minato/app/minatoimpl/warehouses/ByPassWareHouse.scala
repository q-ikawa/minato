package minato.app.minatoimpl.warehouses

import minato.core.cargo.Packet
import minato.core.passage.WareHouse

/**
  * Created by Daiki_Ikawa on 2017/11/11.
  */
class ByPassWareHouse extends WareHouse{
  override def receiveInstruction(instruction: (Seq[Packet]) => Unit) = instruction(_)
}
