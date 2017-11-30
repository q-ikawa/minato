package minato.core.passage

import minato.core.passage.Spot.{Departure, Destination, NilSpot}

/**
  * Created by Daiki_Ikawa on 2017/11/04.
  */

case class Itinerary(spots: Seq[Spot]) {

  implicit val spotIdToDestination: SpotId => Option[Destination] = targetId => spots.find(_.id == targetId).flatMap({
    case d: Destination => Some(d)
    case _ => None
  })

  def cruise(): Unit = for (departure <- departures) departure.preparedSource.execute()

  val departures: Seq[Departure] = spots.collect { case d: Departure => d }
  val routeHasNoDepartures:Boolean = departures.isEmpty
  val routeContainsCycle:Boolean = {
    def containsCycle(spot: Spot, visited: Set[Spot]): Boolean = spot match {
      case NilSpot => false
      case s if visited.contains(s) => true
      case _ => spot.getDestinations.foldLeft(false) { (f, s) => f || containsCycle(s, visited + s) }
    }

    departures.foldLeft(false) { (f, s) => f || containsCycle(s, Set(s)) }
  }

}


