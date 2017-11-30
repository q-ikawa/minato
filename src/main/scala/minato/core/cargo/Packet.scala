package minato.core.cargo

import minato.core.cargo.LabelParts._

/**
  * Created by Daiki_Ikawa on 2017/11/05.
  */
sealed trait Packet {

  def mapDescendant(key: String, f: Packet => Packet): Packet

  def flatMapDescendant(key: String, f: Packet => Option[Packet]): Option[Packet]

  private def get(labelParts: LabelParts): Seq[Packet] = {
    (this, labelParts) match {
      case (ParcelArray(seq), AnyChildren) => seq
      case (ParcelObject(seq), AnyChildren) => seq.map(_._2)

      case (ParcelObject(seq), Index(i)) => if (i < seq.size) Seq(seq(i)._2) else Nil
      case (ParcelObject(seq), Name(s)) => seq.collect({ case (key, packet) if key == s => packet })
      case (ParcelObject(seq), NameWithIndex(s, i)) => {
        val collect = seq.collect({ case (key, packet) if key == s => packet })
        if (collect.size > i) Seq(collect(i)) else Nil
      }

      case (ParcelArray(seq), Index(i)) => if (i < seq.size) Seq(seq(i)) else Nil
      case (ParcelArray(seq), Descendant(s)) => seq.flatMap(p => p.get(Descendant(s)))
      case (ParcelObject(seq), Descendant(s)) => seq.flatMap((kv) => if (kv._1 == s) Seq(kv._2) else kv._2.get(Descendant(s)))

      case _ => Nil
    }
  }

  def get(target: Label): Seq[Packet] = target.parts.foldLeft(Seq(this))((packets, part) => packets.flatMap(_.get(part)))

  def +(label: Label, packet: Packet): Packet = {

    def wrapWith(wrapper: Seq[LabelParts], content: Packet): Packet = wrapper.foldRight(content) {
      case (Descendant(s), c: Packet) => ParcelObject(Seq(s -> c))
      case (NameWithIndex(s, _), c: Packet) => ParcelObject(Seq(s -> c))
      case (Index(_), c: Packet) => ParcelArray(Seq(c))
      case (AnyChildren, c: Packet) => ParcelArray(Seq(c))
      case (Name(s), c: Packet) => ParcelObject(Seq(s -> c))
    }


    def rec(target: Seq[LabelParts], current: Packet, toImport: Packet): Packet = {
      (current, target) match {
        case (_, Nil) => toImport
        case (_: ParcelValue, _ :: _) => wrapWith(target, toImport)

        case (o: ParcelObject, Name(s) :: rest) if o.indexOfKey(s).isDefined => o.mapValueOf(s, p => rec(rest, p, toImport))
        case (o: ParcelObject, Name(s) :: rest) => ParcelObject(o.map :+ (s -> wrapWith(rest, toImport)))
        case (o: ParcelObject, NameWithIndex(s, i) :: rest) if o.indexOfKey(s, i).isDefined => o.mapValueOf(s, p => rec(rest, p, toImport), i)
        case (o: ParcelObject, NameWithIndex(s, i) :: rest) => ParcelObject(o.map :+ (s -> wrapWith(rest, toImport)))
        case (o: ParcelObject, Index(i) :: rest) if i < o.map.length => o.mapValueAt(p => rec(rest, p, toImport), i)
        case (o: ParcelObject, Index(i) :: rest) => ParcelObject(o.map :+ (i.toString -> wrapWith(rest, toImport)))

        case (a: ParcelArray, Name(s) :: rest) => a + ParcelObject(Seq(s -> wrapWith(rest, toImport)))
        case (a: ParcelArray, NameWithIndex(s, _) :: rest) => a + ParcelObject(Seq(s -> wrapWith(rest, toImport)))
        case (a: ParcelArray, Index(i) :: rest) if i < a.list.length => a.mapValue(i, p => rec(rest, p, toImport))
        case (a: ParcelArray, Index(_) :: rest) => a + wrapWith(rest, toImport)

        case (a: ParcelArray, Descendant(s) :: rest) => a.mapDescendant(s, p => rec(rest, p, toImport))
        case (o: ParcelObject, Descendant(s) :: rest) => o.mapDescendant(s, p => rec(rest, p, toImport))

        case (a: ParcelArray, AnyChildren :: rest) => a + wrapWith(rest, toImport)
        case (o: ParcelObject, AnyChildren :: rest) => ParcelObject(o.map :+ ("" -> wrapWith(rest, toImport)))

      }
    }

    rec(label.parts, this, packet)
  }

  def -(label: Label): Option[Packet] = flatMap(label) { _ => None }

  def flatMap(label: Label)(f: Packet => Option[Packet]): Option[Packet] = {
    def rec(target: Seq[LabelParts], current: Packet, f: Packet => Option[Packet]): Option[Packet] = (current, target) match {
      case (p, Nil) => f(p)
      case (v: ParcelValue, _ :: _) => Some(v)

      case (o: ParcelObject, Name(s) :: rest) => o.flatMapValueOf(s, p => rec(rest, p, f))
      case (o: ParcelObject, NameWithIndex(s, i) :: rest) => o.flatMapValueOf(s, p => rec(rest, p, f), i)
      case (o: ParcelObject, Index(i) :: rest) => o.flatMapValueAt(p => rec(rest, p, f), i)

      case (a: ParcelArray, Name(_) :: _) => Some(a)
      case (a: ParcelArray, NameWithIndex(_, _) :: _) => Some(a)
      case (a: ParcelArray, Index(i) :: rest) => a.flatMapValue(i, p => rec(rest, p, f))

      case (a: ParcelArray, AnyChildren :: rest) => a.flatMapValueAll(p => rec(rest, p, f))
      case (o: ParcelObject, AnyChildren :: rest) => o.flatMapValueAll(p => rec(rest, p, f))

      case (a: ParcelArray, Descendant(s) :: rest) => a.flatMapDescendant(s, p => rec(rest, p, f))
      case (o: ParcelObject, Descendant(s) :: rest) => o.flatMapDescendant(s, p => rec(rest, p, f))
    }

    rec(label.parts, this, f)
  }

}

sealed trait ParcelValue extends Packet {
  override def mapDescendant(key: String, f: Packet => Packet): Packet = this

  override def flatMapDescendant(key: String, f: Packet => Option[Packet]): Option[Packet] = Some(this)
}

case class ParcelString(value: String) extends ParcelValue

case object ParcelNull extends ParcelValue

case object ParcelTrue extends ParcelValue

case object ParcelFalse extends ParcelValue

case class ParcelNumber(bigDecimal: BigDecimal) extends ParcelValue

case class ParcelObject(map: Seq[(String, Packet)]) extends Packet {
  def +(kv: (String, Packet)): ParcelObject = ParcelObject(map :+ kv)

  override def mapDescendant(key: String, f: Packet => Packet): Packet = ParcelObject(map.map(kv => if (kv._1 == key) kv._1 -> f(kv._2) else kv._1 -> kv._2.mapDescendant(key, f)))

  override def flatMapDescendant(key: String, f: Packet => Option[Packet]): Option[Packet] = Some(ParcelObject(map.flatMap(kv => if (kv._1 == key) f(kv._2).map(kv._1 -> _) else kv._2.flatMapDescendant(key, f).map(kv._1 -> _))))

  def indexOfKey(key: String, at: Int = 0): Option[Int] = map.zipWithIndex.filter(_._1._1 == key).zipWithIndex.find(_._2 == at).map(_._1._2)

  def mapValueOf(key: String, f: Packet => Packet, at: Int): Packet = indexOfKey(key, at).map(i => ParcelObject((map.take(i) :+ (key -> f(map(i)._2))) ++ map.drop(i + 1))).getOrElse(this)

  def flatMapValueOf(key: String, f: Packet => Option[Packet], at: Int): Option[Packet] = indexOfKey(key, at).map(i => ParcelObject(map.take(i) ++ f(map(i)._2).map(key -> _) ++ map.drop(i + 1))).orElse(Some(this))

  def mapValueAt(f: Packet => Packet, at: Int = 0): Packet = if (map.size > at) ParcelObject((map.take(at) :+ (map(at)._1 -> f(map(at)._2))) ++ map.drop(at + 1)) else this

  def flatMapValueAt(f: Packet => Option[Packet], at: Int = 0): Option[Packet] = if (map.size > at) Some(ParcelObject(map.take(at) ++ f(map(at)._2).map(map(at)._1 -> _) ++ map.drop(at + 1))) else Some(this)

  def mapValueOf(key: String, f: Packet => Packet): Packet = ParcelObject(map.map(kv => if (kv._1 == key) kv._1 -> f(kv._2) else kv))

  def flatMapValueOf(key: String, f: Packet => Option[Packet]): Option[Packet] = Some(ParcelObject(map.flatMap(kv => if (kv._1 == key) f(kv._2).map(kv._1 -> _) else Some(kv))))

  def mapValueAll(f: Packet => Packet): Packet = ParcelObject(map.map(kv => kv._1 -> f(kv._2)))

  def flatMapValueAll(f: Packet => Option[Packet]): Option[Packet] = Some(ParcelObject(map.flatMap(kv => f(kv._2).map(kv._1 -> _))))

}

case class ParcelArray(list: Seq[Packet]) extends Packet {
  def +(value: Packet): ParcelArray = ParcelArray(list :+ value)

  def mapValue(at: Int, f: Packet => Packet): Packet = if (list.size > at) ParcelArray((list.take(at) :+ f(list(at))) ++ list.drop(at + 1)) else this

  def flatMapValue(at: Int, f: Packet => Option[Packet]): Option[Packet] = if (list.size > at) Some(ParcelArray((list.take(at) ++ f(list(at))) ++ list.drop(at + 1))) else Some(this)

  override def mapDescendant(key: String, f: Packet => Packet): Packet = ParcelArray(list.map(a => a.mapDescendant(key, f)))

  override def flatMapDescendant(key: String, f: Packet => Option[Packet]): Option[Packet] = Some(ParcelArray(list.flatMap(a => a.flatMapDescendant(key, f).toSeq)))

  def mapValueAll(f: Packet => Packet): Packet = ParcelArray(list map f)

  def flatMapValueAll(f: Packet => Option[Packet]): Option[Packet] = Some(ParcelArray(list.flatMap(f(_).toSeq)))

}


object Packet {
  def apply(packets: Seq[Packet]): Packet = ParcelArray(packets)
}

case class Label(parts: Seq[LabelParts]) {

  def next: Label = {
    Label(parts.drop(1))
  }

  def /(child: LabelParts): Label = Label(parts :+ child)

}

sealed trait LabelParts {
  def /(parts: LabelParts): Label = Label(this :: parts :: Nil)
}

object LabelParts {

  case object AnyChildren extends LabelParts

  case class Index(i: Int) extends LabelParts

  case class Name(s: String) extends LabelParts

  case class NameWithIndex(s: String, i: Int) extends LabelParts

  case class Descendant(s: String) extends LabelParts

}
