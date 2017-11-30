package minato.algorithms

import scala.annotation.tailrec

/**
  * Created by Daiki_Ikawa on 2017/11/27.
  */
case class DirectedAcyclicGraph[E](vv:Set[Vertex[E]], graph: Set[Edge[E]]) {

  lazy val tsort: Option[Seq[E]] = {
    @tailrec
    def rec(graphSeq: Seq[Edge[E]], res: Seq[E]): Option[Seq[E]] = {
      graphSeq match {
        case Nil => Some(res)
        case g => {
          val edgeTo: Map[TVertex[E], Seq[Edge[E]]] = g.groupBy(_.to)
          val reachableEdges: Seq[TVertex[E]] = edgeTo.filter(to => to._2 == Seq(Edge(EmptyVertex(),to._1))).keys.toSeq
          reachableEdges match {
            case Nil => None
            case r =>
              val next = g.filterNot(edge => r.contains(edge.from)).filterNot(edge => r.contains(edge.to))
              rec(next, res ++ r.collect({case Vertex(v)=> v}))
          }
        }
      }
    }

    val g = graph ++ vv.map(v => Edge(EmptyVertex(),v))
    rec(g.toSeq, Nil)
  }
}

sealed trait TVertex[E] {}

case class EmptyVertex[E]() extends TVertex[E] {}

case class Vertex[E](value: E) extends TVertex[E] {}

case class Edge[E](from: TVertex[E], to: TVertex[E]) {}

