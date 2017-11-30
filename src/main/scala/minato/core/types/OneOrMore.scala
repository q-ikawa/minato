package minato.core.types

/**
  * Created by Daiki_Ikawa on 2017/11/04.
  */
case class OneOrMore[T](values:Seq[T]) {
  def foreach[U](f : T=>(U)) = values.foreach(f)
  def flatMap[R](f : T=>Seq[R] ) = values.flatMap(f)
  def map[R](f : T=>R ) = values.map(f)

  implicit def toSeq:Seq[T] = values

}

object OneOrMore{
  def apply[T](values: Seq[T]): OneOrMore[T] = values match {
    case _::_ => new OneOrMore[T](values)
  }

  implicit def fromSeq[T](implicit seq:Seq[T]):OneOrMore[T] = OneOrMore(seq)

}