package org.mlarocca.s99.graph

import org.scalatest._

class DirectedGraphTest extends FunSpec with Matchers {
  val u = new SimpleVertex[Char, String]('u')
  val v = SimpleVertex('v')
  val u1 = SimpleVertex('u')
  val v1 = SimpleVertex('v')
  val v2 = SimpleVertex(2)
  val v2C = SimpleVertex('2')
  val v2S = SimpleVertex("2")

  val e1 = WeightedEdge[Char](u, u)
  val e1_1 = WeightedEdge[Char](u1, u1)
  val e1L = LabeledEdge(u, u)
  val e2 = WeightedEdge(u, v)
  val e2L = LabeledEdge(u1, v1)
  val e3 = WeightedEdge(v1, v2)
  val e3R = WeightedEdge(v2, v1)

  describe("equality") {
    it("should compare correctly to EmptyGraph") {
      EmptyGraph should equal(EmptyGraph)
      new DirectedGraph[Int, Int]() should not equal (EmptyGraph)
      new DirectedGraph[String, Int]() should not equal (EmptyGraph)
      new DirectedGraph[Any, Nothing]() should not equal (EmptyGraph)
    }

    it("should compare correctly to other DirectedGraph based on vertices") {
      new DirectedGraph[Char, String](Seq(u)) should equal(DirectedGraph(Seq(u1)))
      new DirectedGraph[Char, String](Seq(u, v)) should equal(DirectedGraph(Seq(u1, v1)))
      new DirectedGraph[Char, String](Seq(u)) should not equal (DirectedGraph(Seq(v)))
      new DirectedGraph[Int, String](Seq(v2)) should not equal (DirectedGraph[Char, String](Seq(v2C)))
      new DirectedGraph[String, String](Seq(v2S)) should not equal (DirectedGraph[Char, String](Seq(v2C)))
    }

    it("should compare correctly to other DirectedGraph based on edges") {
      new DirectedGraph[Char, String](Seq(u), Seq(e1)) should equal(DirectedGraph(Seq(u1), Seq(e1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should equal(DirectedGraph(Seq(u1, v1), Seq(e1_1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should not equal(DirectedGraph(Seq(u1, v1), Seq(e2)))
    }
  }

  describe("toString") {
    it("should be [] for EmptyGraph") {
      EmptyGraph.toString() should be("[]")
    }

    it("should be the ordered sequence of edges for a DirectedGraph") {
      new DirectedGraph[Int, Int]().toString() should be ("[]")
      new DirectedGraph[Char, String](Seq(u), Seq(e1)).toString should be("[u - u]")
      DirectedGraph(Seq(u1, v1), Seq(e2)).toString() should equal ("[u > v]")
      DirectedGraph(Seq(u1, v1), Seq(e2)).toString() should equal ("[u > v]")
      DirectedGraph(Seq(u, v, v2), Seq(e1, e2, e3)).toString() should equal ("[u - u, u > v, v > 2]")
    }

    it("should display a symmetric couple of directed edges as an undirected one") {
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)).toString should
          equal(DirectedGraph[Char, String](Seq(u), Seq(e1)).toString())
    }

    it("should only depend on the Graph's edges") {
      new DirectedGraph[Any, String](Seq(v2, v1), Seq(e3, e3R)).toString() should
          equal("[2 - v]")
      new DirectedGraph[Char, String](Seq(u), Seq(e1)).toString should be("[u - u]")
    }
  }
}