package org.mlarocca.s99.graph

import org.scalatest.{Matchers, FunSpec}

class WeightedEdgeTest extends FunSpec with Matchers {

  describe("equality") {
    val u = SimpleVertex('u')
    val v = SimpleVertex('v')
    val u1 = new SimpleVertex('u')
    val v1 = new SimpleVertex('v')

    it ("should match edges with the same vertices and label") {


      WeightedEdge(u, v, "lab") should equal(WeightedEdge(u, v, "lab"))
      WeightedEdge(u, v, "lab") should equal(WeightedEdge(u1, v1, "lab"))
    }

    it ("should not match edges with the same vertices but different label") {
      WeightedEdge(u, v, "lab") should not equal(WeightedEdge(u, v, "lab2"))
      WeightedEdge(u, v, "lab") should not equal(WeightedEdge(u1, v1, "lab3"))
    }

    it ("vertices order should matter") {
      WeightedEdge(u, v, "lab") should not equal(WeightedEdge(v, u, "lab2"))
      WeightedEdge(u, v, "lab") should not equal(WeightedEdge(v1, u1, "lab3"))
    }

    it ("should match vertices despite weight") {
      WeightedEdge(u, v, "lab") should equal(new WeightedEdge(u, v, "lab", 1))
      new WeightedEdge(u, v, "lab", 23) should equal(new WeightedEdge(u1, v1, "lab", 3.0))
    }
  }
}
