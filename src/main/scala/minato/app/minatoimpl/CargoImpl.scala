package minato.app.minatoimpl

import minato.app.infrastructure.{JsonService, XmlService}
import minato.core.cargo.{Packet, ParcelString}
import minato.core.cargo.CargoType.{JSON, TSV, XML}
import minato.core.cargo.{Cargo, CargoType, ParcelString}

/**
  * Created by Daiki_Ikawa on 2017/11/05.
  */
case class CargoImpl(string: String, cargoType: CargoType) extends Cargo {
  override def getPacket: Option[Packet] = cargoType match {
    case XML => XmlService.getPacket(string)
    case JSON => JsonService.getPacket(string)
    case CargoType.String => Some(ParcelString(string))
    case CargoType.PlainText => ???
    case _: TSV => ???
    case _: CargoType.Others => ???
  }
}
case class CargoPacket(packet:Packet) extends Cargo {
  override def cargoType = CargoType.Packet
  override def string = ???
  override def getPacket: Option[Packet] = Some(packet)


}


object CargoImpl {
  def apply(string: String): Cargo = {
    CargoImpl(string, CargoType.PlainText)
  }

  def JSON(string: String): Cargo = CargoImpl(string, CargoType.JSON)
  def String(string: String): Cargo = CargoImpl(string, CargoType.String)

  def Packet(packet:Packet) : Cargo = CargoPacket(packet)

}

