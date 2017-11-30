package minato.core.passage

import minato.app.minatoimpl.CargoImpl
import minato.core.cargo._
import minato.core.passage.Spot.{Destination, Terminal}
import minato.core.types.OneOrMore
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{DiagrammedAssertions, Filter, FlatSpec, FunSpec}
import org.mockito.Mockito.when

import scala.util.Success

/**
  * Created by Daiki_Ikawa on 2017/11/22.
  */
class ManifestsSpec extends FunSpec with DiagrammedAssertions with MockitoSugar {

  describe("Unloader") {
    val cargo1 = CargoImpl.JSON(
      """
        {
        "header" : { "status" : "200", "version" : "v1" },
        "body" : {
          "records" : [
            { "id" : 1,  "title" : "foo" },
            { "id" : 42, "title" : "bar" }
          ],
          "facet" :
            [ { "field" : "id",
                  "values" : { "1" : 1,"42" : 1 }},
              { "field" :"title",
                  "values" : {"foo" : 1, "bar" : 1} } ]
          }}""")
    val cargo2 = CargoImpl.JSON(
      """
        {
        "header" : { "status" : "500", "version" : "v1" },
        "body" : {"message" : "Internal server error"}
        }""")

    val filter = UnaryFilter(Label(Seq(LabelParts.Descendant("status"))), { case ParcelString(s) => s == "200"; case _ => false })

    it(" which has no pre-filters should not filter ") {
      val unloader = Unloader(None, Nil, Nil)
      assert(unloader.unload(cargo1).isDefined)
      assert(unloader.unload(cargo1).forall(_.size == 1))
      assert(unloader.unload(cargo1).forall(_.forall(_.toString == cargo1.getPacket.get.toString)))
      assert(unloader.unload(cargo2).isDefined)
      assert(unloader.unload(cargo2).forall(_.size == 1))
      assert(unloader.unload(cargo2).forall(_.forall(_.toString == cargo2.getPacket.get.toString)))
    }
    it(" which has a pre-filter should filter ") {
      val unloader = Unloader(Some(filter), Nil, Nil)
      assert(unloader.unload(cargo1).isDefined)
      assert(unloader.unload(cargo1).forall(_.size == 1))
      assert(unloader.unload(cargo1).forall(_.forall(_.toString == cargo1.getPacket.get.toString)))
      assert(unloader.unload(cargo2).isEmpty)
    }

    val unpackerRecords = Unpacker(Label(Seq(LabelParts.Descendant("records"))))
    val unpackerFacetField = Unpacker(Label(Seq(LabelParts.Descendant("facet"), LabelParts.AnyChildren, LabelParts.Name("field"))))
    it(" which has multiple unpackers should unpack all ") {
      val unloader = Unloader(Some(filter), Seq(unpackerRecords, unpackerFacetField), Nil)
      assert(unloader.unload(cargo1).isDefined)
      assert(unloader.unload(cargo1).forall(_.size == 3))

      val records = CargoImpl.JSON(""" [{ "id" : 1,  "title" : "foo" },{ "id" : 42, "title" : "bar" }]""").getPacket
      assert(unloader.unload(cargo1).forall(_.contains(records.get)))
      assert(unloader.unload(cargo1).forall(_.contains(ParcelString("id"))))
      assert(unloader.unload(cargo1).forall(_.contains(ParcelString("title"))))

    }

    val unpackerRecord = Unpacker(Label(Seq(LabelParts.Descendant("records"), LabelParts.AnyChildren)))
    val statusDecorator = Decorator(Label(Seq(LabelParts.Name("responseStatus"))), Label(Seq(LabelParts.Descendant("status"))))
    val versionDecorator = Decorator(Label(Seq(LabelParts.Name("apiVersion"))), Label(Seq(LabelParts.Descendant("version"))))
    it(" which has multiple decorators should decorate all ") {
      val unloader = Unloader(Some(filter), Seq(unpackerRecord), Seq(statusDecorator, versionDecorator))
      assert(unloader.unload(cargo1).isDefined)
      assert(unloader.unload(cargo1).forall(_.size == 2))
      val record1 = CargoImpl.JSON("""{ "id" : 1,  "title" : "foo" , "responseStatus": "200", "apiVersion":"v1"}""").getPacket.get
      val record42 = CargoImpl.JSON("""{ "id" : 42,  "title" : "bar" , "responseStatus": "200", "apiVersion":"v1"}""").getPacket.get
      assert(unloader.unload(cargo1).forall(_.contains(record1)))
      assert(unloader.unload(cargo1).forall(_.contains(record42)))
    }

  }

  describe("Loader") {
    it(" should filtered and accumulated appropriately ") {
      val packets = Seq(ParcelTrue, ParcelNull, ParcelFalse)
      val filter = mock[minato.core.cargo.Filter]
      when(filter(ParcelTrue)) thenReturn true
      when(filter(ParcelNull)) thenReturn true
      when(filter(ParcelFalse)) thenReturn false
      val accumulator = new Accumulator {
        override def apply(packet: Packet) = Some(new Cargo {
          override def getPacket = Some(packet)

          override val cargoType: CargoType = CargoType.String
          override val string: String = packet.toString + "hogehoge"
        })
      }
      val loader = Loader(Some(filter), accumulator)
      assert(loader.load(packets).map(_.string).contains(ParcelArray(Seq(ParcelTrue, ParcelNull)).toString + "hogehoge"))
    }
  }


  describe("Manifest") {
    it(" method `>>:` of each destinations is called exact once ") {
      val cargo1 = CargoImpl.JSON(
        """
        {
        "header" : { "status" : "200", "version" : "v1" },
        "body" : {
          "records" : [
            { "id" : 1,  "title" : "foo" },
            { "id" : 42, "title" : "bar" }
          ],
          "facet" :
            [ { "field" : "id",
                  "values" : { "1" : 1,"42" : 1 }},
              { "field" :"title",
                  "values" : {"foo" : 1, "bar" : 1} } ]
          }}""")

      val unpackerRecord = Unpacker(Label(Seq(LabelParts.Descendant("records"), LabelParts.AnyChildren)))
      val statusDecorator = Decorator(Label(Seq(LabelParts.Name("responseStatus"))), Label(Seq(LabelParts.Descendant("status"))))
      val filter = UnaryFilter(Label(Seq(LabelParts.Descendant("status"))), { case ParcelString(s) => s == "200"; case _ => false })

      val unloader = Unloader(Some(filter), Seq(unpackerRecord), Seq(statusDecorator));
      val wareHouse: WareHouse = (instruction) => ((packet) => instruction.apply(packet))
      val accumulator = new Accumulator {
        override def apply(packet: Packet) = Some(CargoImpl.Packet(packet))
      }
      val loader = Loader(None, accumulator)
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
      val consignees: OneOrMore[Consignee] = OneOrMore(Seq(Consignee(loader, OneOrMore(Seq(terminal1,terminal2))), Consignee(loader, OneOrMore(Seq(terminal3, terminal4)))))
      val manifestItem = ManifestItem(unloader, wareHouse, consignees)
      implicit val spotIdToDestination: SpotId => Option[Destination] = {
        case SpotId(x) if x == "terminal1" => Some(terminal1)
        case SpotId(x) if x == "terminal2" => Some(terminal2)
        case SpotId(x) if x == "terminal3" => Some(terminal3)
        case SpotId(x) if x == "terminal4" => Some(terminal4)
        case _ => None
      }
      val manifest = Manifest(None, manifestItem)
      manifest.apply(cargo1)
      val eachAnchorsCalledExactOneTime = Anchor1Called + Anchor2Called + Anchor3Called + Anchor4Called
      assert(x == eachAnchorsCalledExactOneTime)
    }
  }
}
