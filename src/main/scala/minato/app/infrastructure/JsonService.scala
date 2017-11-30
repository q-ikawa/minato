package minato.app.infrastructure

import org.json4s._
import org.json4s.native.JsonMethods._
import minato.core.cargo._

/**
  * Created by Daiki_Ikawa on 2017/11/05.
  */
object JsonService {

  def getPacket(string: String): Option[Packet] = {
    val jsonObject = parse(string)
    Some(jsonToPacket(jsonObject))
  }
  def fromPacket(packet: Packet): String = ???

  def jsonToPacket(jvalue: JValue):Packet = {
    jvalue match {
      case JBool(true)   => ParcelTrue
      case JBool(false)  => ParcelFalse
      case JDouble(n)    => ParcelNumber(n)
      case JDecimal(n)   => ParcelNumber(n)
      case JLong(n)      => ParcelNumber(n)
      case JInt(n)       => ParcelNumber(  BigDecimal.apply(n) )
      case JNull         => ParcelNull
      case JString(null) => ParcelNull
      case JString(s)    => ParcelString(s)
      case JArray(arr)   => ParcelArray( arr.map(jsonToPacket) )
      case JObject(obj)  => ParcelObject( obj.map( kv => kv._1 -> jsonToPacket(kv._2) ) )
      case JSet(set)     => ???
      case JNothing      => ???
    }
  }
}
