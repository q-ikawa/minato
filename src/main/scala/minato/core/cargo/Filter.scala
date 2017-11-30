package minato.core.cargo

import minato.core.cargo.Conditions._


/**
  * Created by Daiki_Ikawa on 2017/11/04.
  */

sealed trait Filter {
  def apply(packet: Packet): Boolean
}

case class OrSet(list: Seq[Filter]) extends Filter {
  override def apply(packet: Packet): Boolean = list.exists(_ apply packet)
}

case class AndSet(list: Seq[Filter]) extends Filter {
  override def apply(packet: Packet): Boolean = list.forall(_ apply packet)
}

case class Not(f: Filter) extends Filter {
  override def apply(packet: Packet): Boolean = !f(packet)
}

sealed trait FilterRule extends Filter

case class UnaryFilter(label: Label, unaryCondition: UnaryCondition) extends FilterRule {
  override def apply(packet: Packet): Boolean = {
    packet.get(label) match {
      case Seq(m: ParcelValue) => unaryCondition(m)
      case _ => false
    }
  }
}

case class LooseUnaryFilter(label: Label, unaryCondition: UnaryCondition) extends FilterRule {
  override def apply(packet: Packet): Boolean = {
    packet.get(label) match {
      case packets:Seq[Packet] => packets.exists({ case x:ParcelValue => unaryCondition(x); case _ => false})
      case _ => false
    }
  }
}

case class BinaryFilter(label1: Label, label2: Label, binaryCondition: BinaryCondition) extends FilterRule {
  override def apply(packet: Packet): Boolean = {
    (packet.get(label1), packet.get(label2)) match {
      case (Seq(m1: ParcelValue), Seq(m2: ParcelValue)) => binaryCondition(m1, m2)
      case _ => false
    }
  }
}

case class LooseBinaryFilter(label1: Label, label2: Label, binaryCondition: BinaryCondition) extends FilterRule {
  override def apply(packet: Packet): Boolean = {
    (packet.get(label1), packet.get(label2)) match {
      case (seq1: Seq[Packet], seq2: Seq[Packet]) =>
        (for(p1 <- seq1; p2<-seq2) yield (p1,p2) ) exists { case (m1: ParcelValue, m2: ParcelValue) => binaryCondition(m1, m2); case _ => false}
      case _ => false
    }
  }
}


case class SetFilter(label: Label, setCondition: SetCondition) extends FilterRule {
  override def apply(packet: Packet): Boolean = setCondition(packet.get(label))
}

case object NoFilters extends FilterRule {
  override def apply(packet: Packet): Boolean = true
}

object Conditions {
  type UnaryCondition = ParcelValue => Boolean
  type BinaryCondition = (ParcelValue, ParcelValue) => Boolean
  type SetCondition = Seq[Packet] => Boolean
}
