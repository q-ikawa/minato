package minato.core.passage

import minato.core.cargo.Cargo
import minato.core.passage.Spot.Destination
import minato.core.types.OneOrMore

import scala.util.Try

/**
  * Created by Daiki_Ikawa on 2017/11/05.
  */

case class SpotId (id: String)

sealed trait Spot {
  val id : SpotId
  def getDestinations: Seq[Spot]
}

object Spot {

  case class Departure(id:SpotId,source:Source,destinations:OneOrMore[Destination]) extends Spot {
    override val getDestinations = destinations.toSeq

    val preparedSource:PreparedSource = source.prepare{
      cargo => for(destination <- destinations) cargo >>: destination
    }

  }

  sealed trait Destination extends Spot {
    val destinationId = id
    def >>:(cargo: Cargo) :Unit

  }

  case class Port(id:SpotId,manifests: OneOrMore[Manifest]) extends Destination {
    override def getDestinations = for(m <- manifests.values; d <- m.destinations) yield d

    override def >>:(cargo: Cargo) = for(m <- manifests) m(cargo)
  }

  case class Terminal(id:SpotId , anchor: TerminalAnchor) extends Destination {
    override def getDestinations = Seq(NilSpot)

    override def >>:(cargo: Cargo) = anchor.moor(cargo)
  }

  case object NilSpot extends Spot {
    val id = SpotId("")
    override def getDestinations = Nil
  }

}

trait Source {
  def prepare(handler: Cargo => Unit):PreparedSource
}

trait PreparedSource {
  def execute():Unit
}

trait TerminalAnchor {
  def moor(cargo: Cargo): Unit
}
