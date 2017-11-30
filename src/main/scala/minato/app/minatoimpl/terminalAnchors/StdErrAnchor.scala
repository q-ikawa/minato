package minato.app.minatoimpl.terminalAnchors

import minato.core.cargo.Cargo
import minato.core.passage.TerminalAnchor

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
class StdErrAnchor extends TerminalAnchor{
  override def moor(cargo: Cargo) = {
    System.err.println(cargo.string)
  }
}
