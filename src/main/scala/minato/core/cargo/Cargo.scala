package minato.core.cargo

/**
  * Created by Daiki_Ikawa on 2017/11/05.
  */
trait Cargo {
  def string: String
  def cargoType: CargoType
  def getPacket: Option[Packet]
}


sealed trait CargoType

object CargoType {

  case object PlainText extends CargoType

  case object String extends CargoType

  case object XML extends CargoType

  case object JSON extends CargoType

  case object Packet extends CargoType

  trait TSV extends CargoType

  object TSV {

    case object TsvWithHeaders extends TSV

    case class TsvNoHeaders(headers: TSVHeaders) extends TSV

  }

  trait TSVHeaders

  trait Others extends CargoType
}


