package org.mlarocca.s99.graph

import org.scalatest._

class DirectedGraphTest extends FunSpec with Matchers {
  val u = new SimpleVertex[Char, String]('u')
  val v = SimpleVertex('v')
  val uTwin = SimpleVertex('u')
  val vTwin = SimpleVertex('v')
  val v2 = SimpleVertex(2)
  val v2C = SimpleVertex('2')
  val v2S = SimpleVertex("2")
  val uS = SimpleVertex("uS")
  val vS = SimpleVertex("vS")

  val e1 = WeightedEdge[Char](u, u)
  val e1S = WeightedEdge[String](uS, uS)
  val e1_1 = WeightedEdge[Char](uTwin, uTwin)
  val e1L = LabeledEdge(u, u)
  val e2 = WeightedEdge(u, v)
  val e2S = WeightedEdge(uS, vS)
  val e2L = LabeledEdge(uTwin, vTwin)
  val e3 = WeightedEdge(vTwin, v2)
  val e3S = WeightedEdge(vS, v2S)
  val e3R = WeightedEdge(v2, vTwin)

  describe("equality") {
    it("should compare correctly to EmptyGraph") {
      EmptyGraph should equal(EmptyGraph)
      new DirectedGraph[Int, Int]() should not equal (EmptyGraph)
      new DirectedGraph[String, Int]() should not equal (EmptyGraph)
      new DirectedGraph[Any, Nothing]() should not equal (EmptyGraph)
    }

    it("should compare correctly to other DirectedGraph based on vertices") {
      new DirectedGraph[Char, String](Seq(u)) should equal(DirectedGraph(Seq(uTwin)))
      new DirectedGraph[Char, String](Seq(u, v)) should equal(DirectedGraph(Seq(uTwin, vTwin)))
      new DirectedGraph[Char, String](Seq(u)) should not equal (DirectedGraph(Seq(v)))
      new DirectedGraph[Int, String](Seq(v2)) should not equal (DirectedGraph[Char, String](Seq(v2C)))
      new DirectedGraph[String, String](Seq(v2S)) should not equal (DirectedGraph[Char, String](Seq(v2C)))
    }

    it("should compare correctly to other DirectedGraph based on edges") {
      new DirectedGraph[Char, String](Seq(u), Seq(e1)) should equal(DirectedGraph(Seq(uTwin), Seq(e1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should equal(DirectedGraph(Seq(uTwin, vTwin), Seq(e1_1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should not equal(DirectedGraph(Seq(uTwin, vTwin), Seq(e2)))
    }
  }

  describe("toString") {

    it("should be [] for EmptyGraph") {
      EmptyGraph.toString() should be("[]")
    }

    it("should be the ordered sequence of edges for a DirectedGraph") {
      new DirectedGraph[Int, Int]().toString() should be ("[]")
      new DirectedGraph[Char, String](Seq(u), Seq(e1)).toString should be("[u - u]")
      DirectedGraph(Seq(uTwin, vTwin), Seq(e2)).toString() should equal ("[u > v]")
      DirectedGraph(Seq(uTwin, vTwin), Seq(e2)).toString() should equal ("[u > v]")
      DirectedGraph(Seq(u, v, v2), Seq(e1, e2, e3)).toString() should equal ("[u - u, u > v, v > 2]")
    }

    it("should display a symmetric couple of directed edges as an undirected one") {
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)).toString should
          equal(DirectedGraph[Char, String](Seq(u), Seq(e1)).toString())
    }

    it("should only depend on the Graph's edges") {
      new DirectedGraph[Any, String](Seq(v2, vTwin), Seq(e3, e3R)).toString() should
          equal("[2 - v]")
      new DirectedGraph[Char, String](Seq(u), Seq(e1)).toString should be("[u - u]")
    }
  }

  describe("fromString") {
    it ("should throw IllegalArgumentException for malformed strings") {
      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("a > b")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a < b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a- b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a -b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a>b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a >b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a > b,,]")
      }
    }

    it("should parse [] into an empty Graph") {
      DirectedGraph.fromString("[]") should equal(new DirectedGraph[String, String]())
      DirectedGraph.fromString("[ ]") should equal(new DirectedGraph[String, String]())
      DirectedGraph.fromString("[  ]") should equal(new DirectedGraph[String, String]())
    }

    it("should parse any ordered sequence of edges into a DirectedGraph") {
      DirectedGraph.fromString("[uS - uS]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS, uS > vS, vS > 2]") should equal(DirectedGraph(Seq(uS, vS, v2S), Seq(e1S, e2S, e3S)))
    }

    it("should be tolerant to redundant spaces") {
      DirectedGraph.fromString("[ uS - uS ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS,  uS > vS , vS > 2 ]") should equal(DirectedGraph(Seq(uS, vS, v2S), Seq(e1S, e2S, e3S)))
    }

    it("should be tolerant to trailing commas") {
      DirectedGraph.fromString("[ uS - uS, ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS,  uS > vS , vS > 2, , ]") should equal(DirectedGraph(Seq(uS, vS, v2S), Seq(e1S, e2S, e3S)))
    }
  }
}