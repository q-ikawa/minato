package minato.app.config

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
object ConfigConstants {
  object ConfigKeys {
    val ID = "name"

    val Departures = "Departures"
    val Ports = "Ports"
    val Terminals = "Terminals"
    val Sources = "Sources"
    val Anchors = "Anchors"
    val Warehouses ="Warehouses"
    val TransShipments = "TransShipments"
    val Externals = "Externals"
  }
  object ConfigKeysTerminal{
    val refId = "anchor-ref"
    val config = "anchor"
  }
  object ConfigKeysDeparture{
    val refId = "source-ref"
    val config = "source"
    val destinations = "destinations"
  }
  object ConfigKeysAnchor{
    val anchorType = "type"
    val extRef = "ext-ref"
  }
  object ConfigKeysSource{
    val sourceType = "type"
    val extRef = "ext-ref"
  }

  object ConfigKeysPort{
    val destinations = "destinations"
  }

}
