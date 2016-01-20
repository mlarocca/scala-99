package org.mlarocca.s99.graph

import org.scalatest.{Matchers, FunSpec}

class LabeledEdgeTest extends FunSpec with Matchers {

  describe("equality") {
    val u = SimpleVertex('u')
    val v = SimpleVertex('v')
    val u1 = new SimpleVertex('u')
    val v1 = new SimpleVertex('v')

    it ("should match edges with the same vertices and label") {
      LabeledEdge(u.key, v.key) should equal(LabeledEdge(u.key, v.key))
      new LabeledEdge(u.key, v.key, "lab") should equal(new LabeledEdge(u.key, v.key, "lab"))
      LabeledEdge(u.key, v.key) should equal(LabeledEdge(u1.key, v1.key))
      new LabeledEdge(u.key, v.key, "lab") should equal(new LabeledEdge(u1.key, v1.key, "lab"))
    }

    it ("should not match edges with the same vertices but different label") {
      new LabeledEdge(u.key, v.key, "lab") should not equal(new LabeledEdge(u.key, v.key, "lab2"))
      new LabeledEdge(u.key, v.key, "lab") should not equal(new LabeledEdge(u1.key, v1.key, "lab3"))
    }

    it ("vertices order should matter") {
      new LabeledEdge(u.key, v.key, "lab") should not equal(new LabeledEdge(v.key, u.key, "lab2"))
      new LabeledEdge(u.key, v.key, 21) should not equal(new LabeledEdge(v1.key, u1.key, 32))
      new LabeledEdge(u.key, v.key, "21") should not equal(new LabeledEdge(v1.key, u1.key, 21))
    }
  }
}

class WeightedEdgeTest extends FunSpec with Matchers {

  describe("equality") {
    val u = SimpleVertex('u')
    val v = SimpleVertex('v')
    val u1 = new SimpleVertex('u')
    val v1 = new SimpleVertex('v')

    it ("should match edges with the same vertices and label") {
      WeightedEdge(u.key, v.key, "lab") should equal(WeightedEdge(u.key, v.key, "lab"))
      WeightedEdge(u.key, v.key, "lab") should equal(WeightedEdge(u1.key, v1.key, "lab"))
    }

    it ("should not match edges with the same vertices but different label") {
      WeightedEdge(u.key, v.key, "lab") should not equal(WeightedEdge(u.key, v.key, "lab2"))
      WeightedEdge(u.key, v.key, "lab") should not equal(WeightedEdge(u1.key, v1.key, "lab3"))
    }

    it ("vertices order should matter") {
      WeightedEdge(u.key, v.key, "lab") should not equal(WeightedEdge(v.key, u.key, "lab2"))
      WeightedEdge(u.key, v.key, "lab") should not equal(WeightedEdge(v1.key, u1.key, "lab3"))
    }

    it ("should match edges despite weight") {
      WeightedEdge(u.key, v.key, "lab") should equal(new WeightedEdge(u.key, v.key, "lab", 1))
      new WeightedEdge(u.key, v.key, "lab", 23) should equal(new WeightedEdge(u1.key, v1.key, "lab", 3.0))
    }

    it ("should match with LabeledEdges despite weights") {
      (new LabeledEdge(u.key, v.key, "lab"): WeightedEdge[Char, String]) should equal(new WeightedEdge(u.key, v.key, "lab", 1))
      (new LabeledEdge(u.key, v.key, "lab"): WeightedEdge[Char, String]) should equal(new WeightedEdge(u.key, v.key, "lab", 0))
    }
  }
}
