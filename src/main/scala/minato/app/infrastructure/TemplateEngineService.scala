package minato.app.infrastructure

import minato.core.cargo._
import org.fusesource.scalate.{Binding, TemplateEngine, TemplateSource}

/**
  * Created by Daiki_Ikawa on 2017/11/05.
  */
object TemplateEngineService {

  val mustache : (Packet,String) => String = (packet,s) => {
    val engine = new TemplateEngine

    def getBindings(packet:Packet): Any = {
      packet match {
        case ParcelArray(arr) => arr.map(getBindings(_))
        case ParcelObject(m) => m.map(kv => kv._1 -> getBindings(kv._2)).toMap
        case ParcelString(s) => s
        case ParcelNumber(n) => n
        case ParcelTrue => true
        case ParcelFalse => false
        case ParcelNull => null
      }
    }

    val bindings = Map("Root" -> getBindings(packet))

    engine.layout(TemplateSource.fromText("dummy.mustache",s"{{#Root}}${s}{{/Root}}"),bindings)
  }
}
