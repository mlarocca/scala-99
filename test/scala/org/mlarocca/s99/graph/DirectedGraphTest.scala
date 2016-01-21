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

  val e1W = WeightedEdge[String](uS.key, uS.key, 5)
  val e3W = WeightedEdge(vS.key, v2S.key, 35.76)

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
      G2.getVertex(vX.key).adj should equal(Seq(e1))
      G2.getVertex(vY.key).adj should equal(Nil)
      G2.getVertex(vZ.key).adj should equal(Nil)
      var G3 = G2.addEdge(e2)
      G3.edges should be(Set(e1, e2))
      G3.getVertex(vX.key).adj should equal(Seq(e1))
      G3.getVertex(vY.key).adj should equal(Seq(e2))
      G3.getVertex(vZ.key).adj should equal(Nil)
      var G4 = G3.addEdge(e3)
      G4.edges should be(Set(e1, e2, e3))
      G4.getVertex(vX.key).adj should equal(Seq(e3, e1))
      G4.getVertex(vY.key).adj should equal(Seq(e2))
      G4.getVertex(vZ.key).adj should equal(Nil)
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
        DirectedGraph.fromString("[()a > b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[(1) a > b]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a > b (]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a > b )]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a > b ()]")
      }

      a[IllegalArgumentException] should be thrownBy {
        DirectedGraph.fromString("[a > b (aa)]")
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

    it("should parse weighted edges") {
      val G1 = DirectedGraph.fromString("[uS - uS (5)]")
      G1 should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1W)))
      G1.edges.head.asInstanceOf[WeightedEdge[String, String]].weight should be(5)
      val G2 = DirectedGraph.fromString("[uS - uS,  uS > vS , vS > 2 (35.76)]")
      G2 should equal(DirectedGraph(Seq(uS, vS, v2S), Seq(e1S, e2S, e3W)))
      G2.edgesFrom(vS.key).head.asInstanceOf[WeightedEdge[String, String]].weight should be(35.76)
    }

    it("should be tolerant to redundant spaces") {
      DirectedGraph.fromString("[ uS - uS ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS,  uS > vS , vS > 2 ]") should equal(DirectedGraph(Seq(uS, vS, v2S), Seq(e1S, e2S, e3S)))
      DirectedGraph.fromString("[ uS - uS   (5), ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1W)))
    }

    it("should be tolerant to trailing commas") {
      DirectedGraph.fromString("[ uS - uS, ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1S)))
      DirectedGraph.fromString("[uS - uS,  uS > vS , vS > 2, , ]") should equal(DirectedGraph(Seq(uS, vS, v2S), Seq(e1S, e2S, e3S)))
      DirectedGraph.fromString("[uS - uS (5), ]") should equal(new DirectedGraph[String, String](Seq(uS), Seq(e1W)))
    }

  }

  describe("reconstructPath") {
    val v1 = "v1"
    val v2 = "v2"
    val v3 = "v3"
    val v4 = "v4"
    val v5 = "v5"

    it("should return a singleton if source and goal match") {
      val predecessors = Map.empty[String, String]
      DirectedGraph.reconstructPath(predecessors)(v3, v3) should be (Some(Seq(v3)))
    }

    it("should reconstruct correct paths") {
      val predecessors = Map(v5 -> v4, v4 -> v2, v3 -> v2, v2 -> v1, v1 -> v1)
      Set[Option[Seq[String]]](Some(Seq(v1, v2, v3, v5)), Some(Seq(v1, v2, v4, v5)))
          .contains(DirectedGraph.reconstructPath(predecessors)(v1, v5)) should be(right = true)
      DirectedGraph.reconstructPath(predecessors)(v1, v4) should be (Some(Seq(v1, v2, v4)))
      DirectedGraph.reconstructPath(predecessors)(v1, v3) should be (Some(Seq(v1, v2, v3)))
      DirectedGraph.reconstructPath(predecessors)(v1, v2) should be (Some(Seq(v1, v2)))
      DirectedGraph.reconstructPath(predecessors)(v4, v5) should be (Some(Seq(v4, v5)))
      val predecessors2 = Map(v5 -> v4, v4->v2, v2 -> v1, v1 -> null)
      DirectedGraph.reconstructPath(predecessors)(v1, v4) should be (Some(Seq(v1, v2, v4)))
    }

    it("should return None if there is no path") {
      val predecessors = Map(v5 -> v4, v4->v2, v2 -> v1, v1 -> null)
      DirectedGraph.reconstructPath(predecessors)(v1, v3) should be (None)
    }
  }

  describe("bfs") {
    val G1 = "[a - b, b - c, b - d, c - e, d - e]": DirectedGraph[String, String]
    val G2 = "[a > b, b > c, b > d, c > e, d > e]": DirectedGraph[String, String]
    val G3 = "[a - c, b - c, c - d, c - f, c - h, d - e, d - f, d - i, f - g, g - i, h - i]": DirectedGraph[String, String]

    val vA = G1.getVertex("a")
    val vB = G1.getVertex("b")
    val vC = G1.getVertex("c")
    val vD = G1.getVertex("d")
    val vE = G1.getVertex("e")

    describe("single source, all destinations") {
      it ("should throw IllegalArgumentException if source vertex is not in the Graph") {
        a[IllegalArgumentException] should be thrownBy {
          G1.bfs("fa")
        }

        a[IllegalArgumentException] should be thrownBy {
          G2.bfs("fa")
        }
      }

      it("should compute the correct distances and predecessors") {
        val SearchResult(distances, predecessors, _) = G2.bfs(vA.key)
        distances should equal(Map(vA -> 0, vB -> 1, vC -> 2, vD -> 2, vE -> 3).map {
          case (k, v) => k.key -> v
        })
        Set(Map(vA -> vA, vB -> vA, vC -> vB, vD -> vB, vE -> vD),
          Map(vA -> vA, vB -> vA, vC -> vB, vD -> vB, vE -> vC)).map {
          _.map {
            case (u, v) => u.key -> v.key
          }
        } should contain(predecessors)
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
        val v1 = G1.getVertex("a").asInstanceOf[SimpleVertex[String, String]]
        a[IllegalArgumentException] should be thrownBy {
          G1.bfs(v1.key, "not here")
        }
      }

      it("should compute the correct distances, predecessors and path") {
        val SearchResult(distances, predecessors, path) = G2.bfs(vA.key, vE.key)

        distances should equal(Map(vA -> 0, vB -> 1, vC -> 2, vD -> 2, vE -> 3).map {
          case (k, v) => k.key -> v
        })

        Set(Map(vA -> vA, vB -> vA, vC -> vB, vD -> vB, vE -> vD),
          Map(vA -> vA, vB -> vA, vC -> vB, vD -> vB, vE -> vC)).map {
          _.map {
            case (u, v) => u.key -> v.key
          }
        } should contain(predecessors)

        Set(Seq(vA, vB, vC, vE),
          Seq(vA, vB, vD, vE)).map {
          _.map(_.key)
        } should contain(path.get)

        G2.bfs(vA.key, vB.key).path.get should be(Seq(vA.key, vB.key))
        G2.bfs(vA.key, vC.key).path.get should be(Seq(vA.key, vB.key, vC.key))

        val SearchResult(distances1, predecessors1, path1) = G3.bfs("a", "i")

        distances1 should equal(
          Map("a" -> 0, "c" -> 1, "b" -> 2, "d" -> 2, "f" -> 2, "h" -> 2, "e" -> 3, "g" -> 3, "i" -> 3)
        )

        predecessors1 should equal(Map("a" -> "a", "b" -> "c", "c" -> "a", "d" -> "c",
          "f" -> "c", "h" -> "c", "e" -> "d", "g" -> "f", "i" -> "h"))

        path1.get should equal(Seq("a", "c", "h", "i"))
      }

    }
  }

  describe("dijkstra") {
    val G1 = "[a - b (2.2), b - c (4.8), b - d (9.8), c - e (456), d - e (8)]": DirectedGraph[String, String]
    val G2 = "[a > b (2.2), b > c (4.8), b > d (9.8), c > e (456), d > e (8)]": DirectedGraph[String, String]
    val G3 = "[a - c (3), b - c (1), c - d (1), c - f (1), c - h (3.5), d - e (1), d - f (1.41), e - i (4), f - g (1.55), g - i (1.55), h - i (4.1)]": DirectedGraph[String, String]

    val vA = G1.getVertex("a")
    val vB = G1.getVertex("b")
    val vC = G1.getVertex("c")
    val vD = G1.getVertex("d")
    val vE = G1.getVertex("e")

    describe("single source, all destinations") {
      it ("should throw IllegalArgumentException if source vertex is not in the Graph") {
        a[IllegalArgumentException] should be thrownBy {
          G1.dijkstra("fa")
        }

        a[IllegalArgumentException] should be thrownBy {
          G2.dijkstra("fa")
        }
      }

      it("should compute the correct distances and predecessors") {
        val SearchResult(distances, predecessors, _) = G2.dijkstra(vA.key)
        distances should equal(Map(vA -> 0, vB -> 2.2, vC -> 7, vD -> 12, vE -> 20).map {
          case (k, v) => k.key -> v
        })

        predecessors should equal(Map(vA -> vA, vB -> vA, vC -> vB, vD -> vB, vE -> vD).map {
          case (k, v) => k.key -> v.key
        })
      }
    }

    describe("single source, single goal") {
      it ("should throw IllegalArgumentException if source vertex is not in the Graph") {
        a[IllegalArgumentException] should be thrownBy {
          G1.dijkstra("no", "neither")
        }

        a[IllegalArgumentException] should be thrownBy {
          G2.dijkstra("no", "nope")
        }
      }

      it ("should throw IllegalArgumentException if goal vertex is not in the Graph") {
        val v1 = G1.getVertex("a")
        a[IllegalArgumentException] should be thrownBy {
          G1.dijkstra(v1.key, "not here")
        }
      }

      it("should compute the correct distances, predecessors and path") {
        val SearchResult(distances, predecessors, path) = G2.dijkstra(vA.key, vE.key)

        distances should equal(Map(vA -> 0, vB -> 2.2, vC -> 7, vD -> 12, vE -> 20).map {
          case (k, v) => k.key -> v
        })

        predecessors should equal(Map(vA -> vA, vB -> vA, vC -> vB, vD -> vB, vE -> vD).map {
          case (k, v) => k.key -> v.key
        })

        path.get should be(Seq(vA, vB, vD, vE).map(_.key))

        G2.dijkstra(vA.key, vB.key).path.get should be(Seq(vA.key, vB.key))
        G2.dijkstra(vA.key, vC.key).path.get should be(Seq(vA.key, vB.key, vC.key))

        val SearchResult(distances1, predecessors1, path1) = G3.dijkstra("a", "i")

        distances1 should equal(
          Map("a" -> 0, "c" -> 3, "b" -> 4, "d" -> 4, "f" -> 4, "h" -> 6.5, "e" -> 5, "g" -> 5.55, "i" -> 7.1)
        )

        predecessors1 should equal(Map("a" -> "a", "b" -> "c", "c" -> "a", "d" -> "c",
          "f" -> "c", "h" -> "c", "e" -> "d", "g" -> "f", "i" -> "g"))

        path1.get should equal(Seq("a", "c", "f", "g", "i"))
      }
    }
  }

  describe("dfs") {
    val G1 = "[a - b (2.2), b - c (4.8), c - d (9.8), d - e (8)]": DirectedGraph[String, String]
    val G2 = "[a > b (2.2), b > c (4.8), c > d (9.8), d > e (8)]": DirectedGraph[String, String]
    val G3 = "[c > a, c > b, c > d, f > c, c > h, d > e, d > f, i > e, f > g, g > i, h > i]": DirectedGraph[String, String]

    val vA = G1.getVertex("a")
    val vB = G1.getVertex("b")
    val vC = G1.getVertex("c")
    val vD = G1.getVertex("d")
    val vE = G1.getVertex("e")

    it ("should throw IllegalArgumentException if source vertex is not in the Graph") {
      a[IllegalArgumentException] should be thrownBy {
        G1.dfs("fa")
      }

      a[IllegalArgumentException] should be thrownBy {
        G2.dfs("fa")
      }
    }

    describe("on a path graph") {
      it("should mark the visiting time correctly for starting from the real source ") {
        val SearchResult(distances, predecessors, _) = G1.dfs(vA.key)
        distances should equal(Map(vA -> 9, vB -> 8, vC -> 7, vD -> 6, vE -> 5).map {
          case (k, v) => k.key -> v
        })
        predecessors should equal(Map(vA -> vA, vB -> vA, vC -> vB, vD -> vC, vE -> vD).map {
          case (k, v) => k.key -> v.key
        })
      }

      it("should mark the visiting time correctly for starting from an intermediate vertex ") {
        val SearchResult(distances, predecessors, _) = G2.dfs(vC.key)
        distances should equal(Map(vA -> 9, vB -> 8, vC -> 5, vD -> 4, vE -> 3).map {
          case (k, v) => k.key -> v
        })
        predecessors should equal(Map(vA -> vA, vB -> vA, vC -> vC, vD -> vC, vE -> vD).map {
          case (k, v) => k.key -> v.key
        })
      }
    }

    describe("on a more complex graph") {
      it("should mark vertices' exit time correctly") {
        val SearchResult(distances, predecessors, _) = G3.dfs(vD.key)
        distances should equal(Map("e" -> 5.0, "f" -> 16.0, "a" -> 14.0, "i" -> 6.0, "b" -> 12.0, "g" -> 7.0, "c" -> 15.0, "h" -> 10.0, "d" -> 17.0))

        predecessors should equal(Map("e" -> "i", "f" -> "d", "a" -> "c", "i" -> "g", "b" -> "c", "g" -> "f", "c" -> "f", "h" -> "c", "d" -> "d"))
      }
    }
  }
}