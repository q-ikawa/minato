package minato.core.cargo

import LabelParts.AnyChildren
import minato.core.passage._
import org.scalatest.{DiagrammedAssertions, FlatSpec}

/**
  * Created by Daiki_Ikawa on 2017/11/06.
  */
class FilterSpec extends FlatSpec with DiagrammedAssertions {

  val packets = ParcelObject(
    Seq(
      "name" -> ParcelString("Studensts List"),
      "student" -> ParcelArray(
        Seq(
          ParcelObject(Seq(
            "name" -> ParcelString("Alice"),
            "age" -> ParcelNumber(15))),
          ParcelObject(Seq(
            "name" -> ParcelString("Bob"),
            "age" -> ParcelNumber(17))
          )
        )
      ),
      "teacher" -> ParcelArray(
        Seq(
          ParcelObject(Seq(
            "name" -> ParcelString("Carol"),
            "age" -> ParcelNumber(35))),
          ParcelObject(Seq(
            "name" -> ParcelString("Dave"),
            "age" -> ParcelNumber(16))
          )
        )
      )
    )
  )

  val unaryFilterTrue: Filter =
    UnaryFilter(Label(Seq(LabelParts.Name("student"), LabelParts.Index(1), LabelParts.Name("age"))), { case ParcelNumber(x) => x == 17; case _ => false })

  val unaryFilterFalse: Filter =
    UnaryFilter(Label(Seq(LabelParts.Name("student"), LabelParts.Index(0), LabelParts.Name("name"))), { case ParcelString(x) => x == "Carol"; case _ => false })

  val looseUnaryFilterTrue: Filter =
    LooseUnaryFilter(Label(Seq(LabelParts.Name("student"),LabelParts.AnyChildren, LabelParts.Name("name"))), { case ParcelString(x) => x == "Bob"; case _ => false })

  val looseUnaryFilterFalse: Filter =
    UnaryFilter(Label(Seq(LabelParts.Name("student"), LabelParts.Index(0), LabelParts.Name("name"))), { case ParcelString(x) => x == "Carol"; case _ => false })

  val binaryFilterFalse: Filter =
    BinaryFilter(Label(Seq(LabelParts.Name("student"), LabelParts.Index(0), LabelParts.Name("age"))),
      Label(Seq(LabelParts.Name("student"), LabelParts.Index(1), LabelParts.Name("age"))), { case (ParcelNumber(x), ParcelNumber(y)) => x == y; case _ => false })

  val binaryFilterTrue: Filter =
    BinaryFilter(Label(Seq(LabelParts.Name("student"), LabelParts.Index(0), LabelParts.Name("name"))),
      Label(Seq(LabelParts.Name("student"), LabelParts.Index(1), LabelParts.Name("name"))), { case (ParcelString(x), ParcelString(y)) => x < y; case _ => false })

  val looseBinaryFilterTrue1: Filter =
    LooseBinaryFilter(Label(Seq(LabelParts.Name("student"),AnyChildren, LabelParts.Name("age"))),
      Label(Seq(LabelParts.Name("teacher"),AnyChildren, LabelParts.Name("age"))), { case (ParcelNumber(x), ParcelNumber(y)) => x < y; case _ => false })
  val looseBinaryFilterTrue2: Filter =
    LooseBinaryFilter(Label(Seq(LabelParts.Name("student"),AnyChildren, LabelParts.Name("age"))),
      Label(Seq(LabelParts.Name("teacher"),AnyChildren, LabelParts.Name("age"))), { case (ParcelNumber(x), ParcelNumber(y)) => x > y; case _ => false })
  val looseBinaryFilterFalse: Filter =
    LooseBinaryFilter(Label(Seq(LabelParts.Name("student"),AnyChildren, LabelParts.Name("age"))),
      Label(Seq(LabelParts.Name("teacher"),AnyChildren, LabelParts.Name("age"))), { case (ParcelNumber(x), ParcelNumber(y)) => x == y; case _ => false })


  val setFilterTrue: Filter =
    SetFilter(Label(Seq(LabelParts.Name("student"),AnyChildren)), { case a: Seq[Packet] => a.size == 2; case _ => false })

  val setFilterFalse: Filter =
    SetFilter(Label(Seq(LabelParts.Name("student"), LabelParts.Index(0))), { case a: Seq[Packet] => a.size == 2; case _ => false })

  "FilterRule" should " return true/false " in {
    assert(unaryFilterTrue apply packets)
    assert((unaryFilterFalse apply packets) == false)
    assert(looseUnaryFilterTrue apply packets)
    assert((looseUnaryFilterFalse apply packets) == false)
    assert((looseBinaryFilterTrue1 apply packets))
    assert((looseBinaryFilterTrue2 apply packets))
    assert((looseBinaryFilterFalse apply packets) == false)
    assert(binaryFilterTrue apply packets)
    assert((binaryFilterFalse apply packets) == false)
    assert(setFilterTrue apply packets)
    assert((setFilterFalse apply packets) == false)
  }

  "(AndSet|OrSet|Not)" should " return true/false" in{
    assert((Not(unaryFilterTrue) apply packets) == false)
    assert((Not(looseBinaryFilterFalse) apply packets) )
    assert((AndSet(Seq(unaryFilterTrue, binaryFilterTrue,setFilterTrue)) apply packets))
    assert((AndSet(Seq(unaryFilterTrue, looseBinaryFilterFalse,setFilterTrue)) apply packets) == false)
    assert((OrSet(Seq(unaryFilterTrue, looseBinaryFilterFalse,setFilterTrue)) apply packets) )
    assert((OrSet(Seq(unaryFilterFalse, looseBinaryFilterFalse,setFilterFalse)) apply packets)  == false)

    assert(Not(OrSet(Seq(Not(AndSet(Seq(unaryFilterTrue, binaryFilterTrue,setFilterTrue))),OrSet(Seq(unaryFilterFalse, looseBinaryFilterFalse,setFilterFalse)))))apply packets)

  }

}
