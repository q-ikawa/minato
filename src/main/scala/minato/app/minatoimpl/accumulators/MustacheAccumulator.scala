package minato.app.minatoimpl.accumulators

import minato.app.infrastructure.TemplateEngineService
import minato.app.minatoimpl.CargoImpl
import minato.core.cargo.Packet
import minato.core.passage.Accumulator

/**
  * Created by Daiki_Ikawa on 2017/11/26.
  */
class MustacheAccumulator(val template:String) extends  Accumulator{

  override def apply(packet: Packet) = {
    Some(CargoImpl.String(TemplateEngineService.mustache(packet,template)))
  }

}
