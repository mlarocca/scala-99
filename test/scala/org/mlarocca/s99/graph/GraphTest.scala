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

  describe("equality") {
    it("should compare correctly to EmptyGraph") {
      EmptyGraph should equal(EmptyGraph)
      new DirectedGraph[Int, Int]() should not equal (EmptyGraph)
      new DirectedGraph[String, Int]() should not equal (EmptyGraph)
      new DirectedGraph[Any, Nothing]() should not equal (EmptyGraph)
    }

    it("should compare correctly to other DirectedGraph based on vertices") {
      new DirectedGraph[Char, String](Seq(u)) should equal (DirectedGraph(Seq(u1)))
      new DirectedGraph[Char, String](Seq(u, v)) should equal (DirectedGraph(Seq(u1, v1)))
      new DirectedGraph[Char, String](Seq(u)) should not equal (DirectedGraph(Seq(v)))
      new DirectedGraph[Int, String](Seq(v2)) should not equal (DirectedGraph[Char, String](Seq(v2C)))
      new DirectedGraph[String, String](Seq(v2S)) should not equal (DirectedGraph[Char, String](Seq(v2C)))
    }

    it("should compare correctly to other DirectedGraph based on edges") {
      new DirectedGraph[Char, String](Seq(u), Seq(e1)) should equal (DirectedGraph(Seq(u1), Seq(e1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should equal (DirectedGraph(Seq(u1, v1), Seq(e1_1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should not equal (DirectedGraph(Seq(u1, v1), Seq(e2)))
    }
  }
}