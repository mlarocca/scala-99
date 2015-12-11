package org.mlarocca.s99.tree

import org.scalatest._

import scala.util.Random

class BinaryTreeTest extends FunSpec with Matchers {

  describe("equality") {
    it("should compare correctly to empty trees") {
      Leaf should equal(Leaf)
      BinaryNode(1) should not equal(Leaf)
    }

    it ("should return true for two roots with the same key") {
      BinaryNode(1) should equal(BinaryNode(1))
      BinaryNode(true) should equal(BinaryNode(true))
      BinaryNode(false) should equal(BinaryNode(false))
      BinaryNode("abcd") should equal(BinaryNode("abcd"))
    }

    it ("should return false for two roots with different keys") {
      BinaryNode(Random.nextInt()) should not equal (BinaryNode(-1))
      BinaryNode(1 + Random.nextInt()) should not equal (BinaryNode(0))
      BinaryNode(1) should not equal(BinaryNode(2))
      BinaryNode("1") should not equal(BinaryNode("2"))
      BinaryNode("abc") should not equal(BinaryNode("abcd"))
      BinaryNode("abcd") should not equal(BinaryNode("abc"))
      BinaryNode(true) should not equal(BinaryNode(false))
      BinaryNode(true) should not equal(BinaryNode(1))
    }

    it("should compare correctly deeper trees") {
      BinaryNode(1, BinaryNode(2), BinaryNode(3)) should equal(BinaryNode(1, BinaryNode(2), BinaryNode(3)))
      BinaryNode(1, Leaf, Leaf) should equal(BinaryNode(1))

      BinaryNode(1, BinaryNode(2), Leaf) should not equal(BinaryNode(1, BinaryNode(2), BinaryNode(3)))
      BinaryNode(2, BinaryNode(1), BinaryNode(3)) should not equal(BinaryNode(1, BinaryNode(2), BinaryNode(3)))
      BinaryNode(1, BinaryNode(3), BinaryNode(2)) should not equal(BinaryNode(1, BinaryNode(2), BinaryNode(3)))
      BinaryNode(1, Leaf, Leaf) should not equal(BinaryNode(1, Leaf, BinaryNode(3)))
    }
  }


}
