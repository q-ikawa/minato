package minato.core.passage

import minato.app.minatoimpl.CargoImpl
import minato.app.minatoimpl.warehouses.ByPassWareHouse
import minato.core.cargo.{Cargo, Packet}
import minato.core.passage.Spot.{Departure, Destination, Port, Terminal}
import minato.core.types.OneOrMore
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{DiagrammedAssertions, FlatSpec, FunSpec}

/**
  * Created by Daiki_Ikawa on 2017/11/22.
  */
class ItinerariesSpec extends FunSpec with DiagrammedAssertions with MockitoSugar {

  var x = 0
  val Anchor1Called = 1;
  val Anchor2Called = 100;
  val Anchor3Called = 10000;
  val Anchor4Called = 1000000;
  val anchor1: TerminalAnchor = c => x += Anchor1Called
  val anchor2: TerminalAnchor = c => x += Anchor2Called
  val anchor3: TerminalAnchor = c => x += Anchor3Called
  val anchor4: TerminalAnchor = c => x += Anchor4Called
  val terminal1 = Terminal(SpotId("terminal1"), anchor1)
  val terminal2 = Terminal(SpotId("terminal2"), anchor2)
  val terminal3 = Terminal(SpotId("terminal3"), anchor3)
  val terminal4 = Terminal(SpotId("terminal4"), anchor4)

  val accumulator = new Accumulator {
    override def apply(packet: Packet) = Some(CargoImpl.Packet(packet))
  }
  val loader = Loader(None, accumulator)
  val unloader = Unloader(None, Nil, Nil)
  val byPassWareHouse = new ByPassWareHouse
  val manifest12 = Manifest(None, ManifestItem(unloader, byPassWareHouse, OneOrMore(Seq(Consignee(loader, OneOrMore(Seq(terminal1, terminal2)))))))

  def portTo(spotId:SpotId,destinations: OneOrMore[Destination]) = Port(spotId,OneOrMore(Seq(Manifest(None, ManifestItem(unloader, byPassWareHouse, OneOrMore(Seq(Consignee(loader, destinations))))))))

  describe("itinerary.cruise ") {
    it(" should call each terminalAnchors.moor ") {

      val portA = portTo(SpotId("portA"),OneOrMore(Seq(terminal1,terminal2)))
      val portB = portTo(SpotId("portB"),OneOrMore(Seq(portA)))
      val portC = portTo(SpotId("portC"),OneOrMore(Seq(terminal3)))
      val portX = portTo(SpotId("portX"),OneOrMore(Seq(terminal4)))
      val portY = portTo(SpotId("portY"),OneOrMore(Seq(portX)))
      val portZ = portTo(SpotId("portZ"),OneOrMore(Seq(portX)))

      val source1 = new Source {
        val cargo = CargoImpl.JSON("""{"source":"1"}""")

        override def prepare(handler: (Cargo) => Unit) = () =>{ handler.apply(cargo)}
      }
      val source2 = new Source {
        val cargo = CargoImpl.JSON("""{"source":"2"}""")

        override def prepare(handler: (Cargo) => Unit) = () => handler.apply(cargo)
      }
      val source3 = new Source {
        val cargo = CargoImpl.JSON("""{"source":"3"}""")

        override def prepare(handler: (Cargo) => Unit) = () => handler.apply(cargo)
      }
      val departureBC = Departure(SpotId("departureBC"),source1, OneOrMore(Seq(portB, portC)))
      val departureY = Departure(SpotId("departureY"),source2, OneOrMore(Seq(portY)))
      val departureZ = Departure(SpotId("departureZ"),source3, OneOrMore(Seq(portZ)))

      val route = Itinerary(Seq(portA, portB, portC, portX, portY, portZ, departureBC, departureY, departureZ,terminal1,terminal2,terminal3,terminal4))

      route.cruise()
      val Anchor4CalledTwice = Anchor4Called * 2
      assert(x == Anchor1Called + Anchor2Called + Anchor3Called + Anchor4CalledTwice)
      assert(route.routeHasNoDepartures == false)
    }
  }

  describe("Itinerary without any departures ") {
    it(" itinerary.routHasNoDepartures should be true") {
      val portA = portTo(SpotId("portA"),OneOrMore(Seq(terminal1,terminal2)))
      val portB = portTo(SpotId("portB"),OneOrMore(Seq(portA)))
      val portC = portTo(SpotId("portC"),OneOrMore(Seq(terminal3)))
      val portX = portTo(SpotId("portX"),OneOrMore(Seq(terminal4)))
      val portY = portTo(SpotId("portY"),OneOrMore(Seq(portX)))
      val portZ = portTo(SpotId("portZ"),OneOrMore(Seq(portX)))


      val route = Itinerary(Seq(portA, portB, portC, portX, portY, portZ,terminal1,terminal2,terminal3,terminal4))
      assert(route.routeHasNoDepartures == true)
    }
  }

  describe("Itinerary with cycle ") {
    it(" itinerary.routContainsCycle should be true") {
      //
    }
  }

}
