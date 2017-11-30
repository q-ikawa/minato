package minato.core.cargo

import minato.core.passage._
import org.scalatest.{DiagrammedAssertions, FunSpec}

/**
  * Created by Daiki_Ikawa on 2017/11/06.
  */
class PacketSpec extends FunSpec with DiagrammedAssertions {

  val N = LabelParts.Name
  val I = LabelParts.Index
  val Ni = LabelParts.NameWithIndex
  val D = LabelParts.Descendant
  val L = Label

  val O = ParcelObject
  val A = ParcelArray
  val True = ParcelTrue
  val False = ParcelFalse
  val Null = ParcelNull
  val R = ParcelNumber
  val S = ParcelString


  describe("{a:x,b:y,c:[v,w],d:[{b:c,x:z},{x:{v:w}}]}") {
    val `{a:x,b:y,c:[v,w],d:[{b:c,x:z},{x:{v:w}}]}` = ParcelObject(
      Seq("a" -> ParcelString("x"),
        "b" -> ParcelString("y"),
        "c" -> ParcelArray(Seq(ParcelString("v"), ParcelString("w"))),
        "d" -> ParcelArray(Seq(
          ParcelObject(Seq("b" -> ParcelString("c"), "x" -> ParcelString("z"))),
          ParcelObject(Seq("x" -> ParcelObject(Seq("v" -> ParcelString("w"))))
          )))))

    it("get(a) should be x") {
      val `a` = Label(Seq(LabelParts.Name("a")))
      assert((`{a:x,b:y,c:[v,w],d:[{b:c,x:z},{x:{v:w}}]}` get `a`) == Seq(ParcelString("x")))
    }

    it("get(c/1) should be w") {
      val `c/1` = Label(Seq(LabelParts.Name("c"), LabelParts.Index(1)))
      assert((`{a:x,b:y,c:[v,w],d:[{b:c,x:z},{x:{v:w}}]}` get `c/1`) == Seq(ParcelString("w")))
    }

    it("get(d//x) should be [z,[v:w]]") {
      val `d//x` = Label(Seq(LabelParts.Name("d"), LabelParts.Descendant("x")))
      assert((`{a:x,b:y,c:[v,w],d:[{b:c,x:z},{x:{v:w}}]}` get `d//x`) == (Seq(ParcelString("z"), ParcelObject(Seq("v" -> ParcelString("w"))))))
    }
  }

  it("{a:[x,y,z]}.get(a/0) should be x") {
    val packet = ParcelObject(Seq("a" -> ParcelArray(Seq(ParcelString("x"), ParcelString("y"), ParcelString("z")))))
    val `a/0` = L(Seq(N("a"), I(0)))
    assert(packet.get(`a/0`) == Seq(ParcelString("x")))
  }

  it("{a:x}.get(a/0) should be x") {
    val packet = ParcelObject(Seq("a" -> ParcelArray(Seq(ParcelString("x")))))
    val `a/0` = Label(Seq(LabelParts.Name("a"), LabelParts.Index(0)))
    assert(packet.get(`a/0`) == Seq(ParcelString("x")))
  }

  it("{a:[x,y,z]}.get(a) should be [x,y,z]") {
    val packet = ParcelObject(Seq("a" -> ParcelArray(Seq(ParcelString("x"), ParcelString("y"), ParcelString("z")))))
    val `a` = Label(Seq(LabelParts.Name("a")))
    assert(packet.get(`a`) == Seq(ParcelArray(Seq(ParcelString("x"), ParcelString("y"), ParcelString("z")))))
  }

  it("{a:[x]}.get(a) should be x") {
    val packet = ParcelObject(Seq("a" -> ParcelArray(Seq(ParcelString("x")))))
    val `a` = Label(Seq(LabelParts.Name("a")))
    assert(packet.get(`a`) == Seq(ParcelArray(Seq(ParcelString("x")))))
  }

}
