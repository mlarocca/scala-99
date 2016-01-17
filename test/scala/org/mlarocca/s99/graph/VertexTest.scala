package org.mlarocca.s99.graph

import org.scalatest.{Matchers, FunSpec}

class VertexTest extends FunSpec with Matchers {

  describe("equality") {
    it ("should match vertices with the same key and type") {
      SimpleVertex('a') should equal(SimpleVertex('a'))
      SimpleVertex('a') should not equal(SimpleVertex("a"))
      SimpleVertex('1') should not equal(SimpleVertex(1))
      SimpleVertex(1) should not equal(SimpleVertex(1.0))
    }

    it ("should match vertices despite adjacentcy list") {
      new SimpleVertex('a', Seq(WeightedEdge(null, null))) should equal(SimpleVertex('a'))
      new SimpleVertex(1, Seq(WeightedEdge[Any](SimpleVertex('a'), SimpleVertex(1)))) should equal(SimpleVertex(1))
    }
  }
}
