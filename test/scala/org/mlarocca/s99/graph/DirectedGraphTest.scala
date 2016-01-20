package org.mlarocca.s99.graph

import org.scalatest._
import DirectedGraph.{EmptyGraph, SearchResult}

class DirectedGraphTest extends FunSpec with Matchers {
  val u = new SimpleVertex[Char, String]('u')
  val v = SimpleVertex('v')
  val uTwin = SimpleVertex('u')
  val vTwin = SimpleVertex('v')
  val v2 = SimpleVertex(2)
  val v2Twin = SimpleVertex('2')
  val v2C = SimpleVertex('2')
  val v2S = SimpleVertex("2")
  val uS = SimpleVertex("uS")
  val vS = SimpleVertex("vS")

  val e1 = WeightedEdge[Char](u.key, u.key)
  val e1S = WeightedEdge[String](uS.key, uS.key)
  val e1_1 = WeightedEdge[Char](uTwin.key, uTwin.key)
  val e1L = LabeledEdge(u.key, u.key)
  val e2 = WeightedEdge(u.key, v.key)
  val e2S = WeightedEdge(uS.key, vS.key)
  val e2L = LabeledEdge(uTwin.key, vTwin.key)
  val e3 = WeightedEdge(vTwin.key, v2.key)
  val e3S = WeightedEdge(vS.key, v2S.key)
  val e3R = WeightedEdge(v2.key, vTwin.key)
  val e3RS = WeightedEdge(v2S.key, vS.key)

  describe("equality") {
    it("should compare correctly to EmptyGraph") {
      EmptyGraph[String, String] should equal(EmptyGraph[Int, Int])
      new DirectedGraph[Int, Int]() should equal(EmptyGraph[Int, Int])
      new DirectedGraph[String, Int]() should equal(EmptyGraph[String, Int])
      new DirectedGraph[Nothing, Nothing]() should equal(EmptyGraph[Float, Double])
      "[a - b]" should not equal EmptyGraph[String, String]
    }

    it("should compare correctly to other DirectedGraph based on vertices") {
      new DirectedGraph[Char, String](Seq(u)) should equal(DirectedGraph(Seq(uTwin)))
      new DirectedGraph[Char, String](Seq(u, v)) should equal(DirectedGraph(Seq(uTwin, vTwin)))
      new DirectedGraph[Char, String](Seq(u)) should not equal DirectedGraph(Seq(v))
      new DirectedGraph[Int, String](Seq(v2)) should not equal DirectedGraph[Char, String](Seq(v2C))
      new DirectedGraph[String, String](Seq(v2S)) should not equal DirectedGraph[Char, String](Seq(v2C))
    }

    it("should compare correctly to other DirectedGraph based on edges") {
      new DirectedGraph[Char, String](Seq(u), Seq(e1)) should equal(DirectedGraph(Seq(uTwin), Seq(e1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should equal(DirectedGraph(Seq(uTwin, vTwin), Seq(e1_1)))
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)) should not equal DirectedGraph(Seq(uTwin, vTwin), Seq(e2))
    }
  }

  describe("addVertex") {
    it ("should throw IllegalArgumentException for vertices already in the Graph") {
      a[IllegalArgumentException] should be thrownBy {
        new DirectedGraph[String, String](Seq(SimpleVertex("x"))).addVertex(SimpleVertex("x"))
      }
    }

    it ("should add new vertices") {
      val G1 = new DirectedGraph[String, String](Seq(SimpleVertex("x"))).addVertex(SimpleVertex("y"))
      G1.vertices should equal(Set(SimpleVertex("x"), SimpleVertex("y")))
      val G2 = G1.addVertex(SimpleVertex("z"))
      G2.vertices should equal(Set(SimpleVertex("x"), SimpleVertex("y"), SimpleVertex("z")))
    }
  }

  describe("addEdge") {
    it ("should throw IllegalArgumentException for vertices not in the Graph") {
      a[IllegalArgumentException] should be thrownBy {
        EmptyGraph[String, String].addEdge(WeightedEdge("x", "x"))
      }
    }

    it ("should add new edges") {
      val Seq(vX, vY, vZ) = Seq(SimpleVertex("x"), SimpleVertex("y"), SimpleVertex("z"))
      val e1 = WeightedEdge(vX.key, vY.key)
      val e2 = WeightedEdge(vY.key, vZ.key)
      val e3 = WeightedEdge(vX.key, vZ.key)
      val G1 = new DirectedGraph[String, String]().addVertex(vX).addVertex(vY).addVertex(vZ)
      var G2 = G1.addEdge(e1)
      G2.edges should be(Set(e1))
      G2.getVertex(vX.key).get.adj should equal(Seq(e1))
      G2.getVertex(vY.key).get.adj should equal(Nil)
      G2.getVertex(vZ.key).get.adj should equal(Nil)
      var G3 = G2.addEdge(e2)
      G3.edges should be(Set(e1, e2))
      G3.getVertex(vX.key).get.adj should equal(Seq(e1))
      G3.getVertex(vY.key).get.adj should equal(Seq(e2))
      G3.getVertex(vZ.key).get.adj should equal(Nil)
      var G4 = G3.addEdge(e3)
      G4.edges should be(Set(e1, e2, e3))
      G4.getVertex(vX.key).get.adj should equal(Seq(e3, e1))
      G4.getVertex(vY.key).get.adj should equal(Seq(e2))
      G4.getVertex(vZ.key).get.adj should equal(Nil)
    }

  }

  describe("toString") {
    it("should be [] for EmptyGraph") {
      EmptyGraph[Nothing, Nothing].toString() should be("[]")
    }

    it("should be the ordered sequence of edges for a DirectedGraph") {
      new DirectedGraph[Int, Int]().toString() should be ("[]")
      new DirectedGraph[Char, String](Seq(u), Seq(e1)).toString should be("[u - u]")
      DirectedGraph(Seq(uTwin, vTwin), Seq(e2)).toString() should equal ("[u > v]")
      DirectedGraph(Seq(uTwin, vTwin), Seq(e2)).toString() should equal ("[u > v]")
      //new DirectedGraph[AnyVal, String](Seq(u, v, v2), Seq(e1, e2, e3)).toString() should equal ("[u - u, u > v, v > 2]")
    }

    it("should display a symmetric couple of directed edges as an undirected one") {
      new DirectedGraph[Char, String](Seq(u, v), Seq(e1)).toString should
          equal(DirectedGraph[Char, String](Seq(u), Seq(e1)).toString())
    }

    it("should only depend on the Graph's edges") {
      new DirectedGraph[String, String](Seq(v2S, vS), Seq(e3RS, e3S)).toString() should
          equal("[2 - vS]")
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

  describe("reconstructPath") {
    val v1 = new SimpleVertex[String, String]("v1")
    val v2 = new SimpleVertex[String, String]("v2")
    val v3 = new SimpleVertex[String, String]("v3")
    val v4 = new SimpleVertex[String, String]("v4")
    val v5 = new SimpleVertex[String, String]("v5")

    it("should return a singleton if source and goal match") {
      val predecessors = Map.empty[SimpleVertex[String, String], SimpleVertex[String, String]]
      DirectedGraph.reconstructPath(v3, v3, predecessors) should be (Some(Seq(v3)))
    }

    it("should reconstruct correct paths") {
      val predecessors = Map(v5 -> v4, v4->v2, v3->v2, v2 -> v1, v1 -> null)
      Set[Option[Seq[SimpleVertex[String, String]]]](Some(Seq(v1, v2, v3, v5)), Some(Seq(v1, v2, v4, v5)))
          .contains(DirectedGraph.reconstructPath(v1, v5, predecessors)) should be(right = true)
      DirectedGraph.reconstructPath(v1, v4, predecessors) should be (Some(Seq(v1, v2, v4)))
      DirectedGraph.reconstructPath(v1, v3, predecessors) should be (Some(Seq(v1, v2, v3)))
      DirectedGraph.reconstructPath(v1, v2, predecessors) should be (Some(Seq(v1, v2)))
      DirectedGraph.reconstructPath(v4, v5, predecessors) should be (Some(Seq(v4, v5)))
      val predecessors2 = Map(v5 -> v4, v4->v2, v2 -> v1, v1 -> null)
      DirectedGraph.reconstructPath(v1, v4, predecessors2) should be (Some(Seq(v1, v2, v4)))
    }

    it("should return None if there is no path") {
      val predecessors = Map(v5 -> v4, v4->v2, v2 -> v1, v1 -> null)
      DirectedGraph.reconstructPath(v1, v3, predecessors) should be (None)
    }
  }

  describe("bfs") {
    val G1 = "[a - b, b - c, b - d, c - e, d - e]": DirectedGraph[String, String]
    val G2 = "[a > b, b > c, b > d, c > e, d > e]": DirectedGraph[String, String]
    val vA = G1.getVertex("a").get
    val vB = G1.getVertex("b").get
    val vC = G1.getVertex("c").get
    val vD = G1.getVertex("d").get
    val vE = G1.getVertex("e").get

    describe("single source, all destinations") {
      it ("should throw IllegalArgumentException if source vertex is not in the Graph") {
        a[IllegalArgumentException] should be thrownBy {
          G1.bfs("fa")
        }

        a[IllegalArgumentException] should be thrownBy {
          G2.bfs("fa")
        }
      }

      it("should compute the correct distances") {
        val SearchResult(distances, predecessors, _) = G2.bfs(vA.key)
        println(G2.edges.foreach(println(_)))
        distances should equal(Map(vA -> 0, vB -> 1, vC -> 2, vD -> 2, vE -> 3).map {
          case (k, v) => k.key -> v
        })
        Set(Map(vA -> null, vB -> vA, vC -> vB, vD -> vB, vE -> vD),
          Map(vA -> null, vB -> vA, vC -> vB, vD -> vB, vE -> vC).map {
            case (u,v) => u.key -> v.key
          }) should contain(predecessors)
      }
    }

    describe("single source, single goal") {
      it ("should throw IllegalArgumentException if source vertex is not in the Graph") {
        a[IllegalArgumentException] should be thrownBy {
          G1.bfs("no", "neither")
        }

        a[IllegalArgumentException] should be thrownBy {
          G2.bfs("no", "nope")
        }
      }

      it ("should throw IllegalArgumentException if goal vertex is not in the Graph") {
        val v1 = G1.getVertex("a").get.asInstanceOf[SimpleVertex[String, String]]
        a[IllegalArgumentException] should be thrownBy {
          G1.bfs(v1.key, "not here")
        }
      }
    }
  }

}