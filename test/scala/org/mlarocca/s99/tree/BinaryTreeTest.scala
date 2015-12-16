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

  describe("cBalanced") {
    it ("should throw IllegalArgumentException for negative Int") {
      a[IllegalArgumentException] should be thrownBy {
        BinaryTree.cBalanced(-1, "x")
      }
    }

    it ("should return a leaf for n == 0") {
      BinaryTree.cBalanced(0, "x") should be(Seq(Leaf))
    }

    it ("should return a single node for n == 1") {
      BinaryTree.cBalanced(1, "x") should be(Seq(BinaryNode("x")))
    }

    it ("should return two possible solutions for n == 2") {
      BinaryTree.cBalanced(2, "x") should be(Seq(new BinaryNode("x", BinaryNode("x"), Leaf), new BinaryNode("x", Leaf, BinaryNode("x"))))
    }

    it ("should return a single possible solutions for n == 3") {
      BinaryTree.cBalanced(3, "x") should be(Seq(new BinaryNode("x", BinaryNode("x"), BinaryNode("x"))))
    }

    it ("should return all possible solutions for n == 4") {
      BinaryTree.cBalanced(4, "x") should equal(Seq(
        new BinaryNode("x", new BinaryNode("x", BinaryNode("x"), Leaf), BinaryNode("x")),
        new BinaryNode("x", new BinaryNode("x", Leaf, BinaryNode("x")), BinaryNode("x")),
        new BinaryNode("x", BinaryNode("x"), new BinaryNode("x", BinaryNode("x"), Leaf)),
        new BinaryNode("x", BinaryNode("x"), new BinaryNode("x", Leaf, BinaryNode("x")))
      ))
    }
  }

  describe("isSymmetric") {
    it("should be true for leaves") {
      Leaf.isSymmetric() should be(true)
    }

    it ("should return true for symmetric trees") {
      BinaryNode(1).isSymmetric() should be(true)
      BinaryNode(1, BinaryNode(2), BinaryNode(2)).isSymmetric() should be(true)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).isSymmetric() should be(true)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)), BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(5)), BinaryNode(3))).isSymmetric() should be(true)
    }

    it ("should return true for asymmetric trees") {
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, BinaryNode(3), Leaf)).isSymmetric() should be(false)
      BinaryNode(1, BinaryNode(2, BinaryNode(4), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).isSymmetric() should be(false)
      BinaryNode(1, BinaryNode(2, BinaryNode(4), BinaryNode(2, BinaryNode(4), Leaf)), BinaryNode(2, BinaryNode(2, BinaryNode(4), Leaf), BinaryNode(3))).isSymmetric() should be(false)
    }
  }
}
