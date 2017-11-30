package minato.app.minatoimpl.terminalAnchors

import minato.core.cargo.Cargo
import minato.core.passage.TerminalAnchor

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
class StdOutAnchor extends TerminalAnchor{
  override def moor(cargo: Cargo) = {
    println(cargo.string)
  }
}
