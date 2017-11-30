package minato.algorithms

import org.scalatest.{DiagrammedAssertions, FunSpec}

/**
  * Created by Daiki_Ikawa on 2017/11/27.
  */
class DirectedAcyclicGraphSpec extends FunSpec with DiagrammedAssertions{

  describe("tsort ") {
    it(" serial data1 should be sorted"){
      val dag = DirectedAcyclicGraph(Set(Vertex(1),Vertex(2),Vertex(3),Vertex(4),Vertex(5)),Set(
        Edge(Vertex(1),Vertex(2)),
        Edge(Vertex(2),Vertex(3)),
        Edge(Vertex(3),Vertex(4)),
        Edge(Vertex(4),Vertex(5))
      ))
      dag.tsort match{
        case Some(Seq(1,2,3,4,5))  =>
        case Some(_) => fail("??")
        case None => fail("???")
      }
    }
    it(" serial data2 should be sorted"){
      val dag = DirectedAcyclicGraph(Set(Vertex(1),Vertex(2),Vertex(3),Vertex(4),Vertex(5)),Set(
        Edge(Vertex(5),Vertex(4)),
        Edge(Vertex(4),Vertex(3)),
        Edge(Vertex(3),Vertex(2)),
        Edge(Vertex(2),Vertex(1))
      ))
      dag.tsort match{
        case Some(Seq(5,4,3,2,1))  =>
        case Some(_) => fail("??")
        case None => fail("???")
      }
    }

    it("cyclic data should return None"){
      val dag = DirectedAcyclicGraph(Set(Vertex(1),Vertex(2),Vertex(3),Vertex(4),Vertex(5)),Set(
        Edge(Vertex(5),Vertex(4)),
        Edge(Vertex(4),Vertex(3)),
        Edge(Vertex(3),Vertex(2)),
        Edge(Vertex(2),Vertex(4)),
        Edge(Vertex(2),Vertex(1))
      ))
      dag.tsort match{
        case Some(_) => fail("??")
        case None => ()
      }
    }
  }

}
