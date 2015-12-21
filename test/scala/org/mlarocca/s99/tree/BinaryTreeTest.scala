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

      new BinaryNode("x", new BinaryNode("x", Leaf, BinaryNode("x")), new BinaryNode("x", BinaryNode("x"), Leaf)) should not
      equal(new BinaryNode("x", new BinaryNode("x", Leaf, BinaryNode("x")), new BinaryNode("x", Leaf, BinaryNode("x"))))
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

  describe("hasSymmetricStructure") {
    it("should be true for leaves") {
      Leaf.hasSymmetricStructure() should be(true)
    }

    it ("should return true for symmetric trees") {
      BinaryNode(1).hasSymmetricStructure() should be(true)
      BinaryNode(1, BinaryNode(2), BinaryNode(2)).hasSymmetricStructure() should be(true)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).hasSymmetricStructure() should be(true)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)), BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(5)), BinaryNode(3))).hasSymmetricStructure() should be(true)
    }
    it ("should return true for trees with a symmetric structure, despite asymmetric values") {
      BinaryNode(44, BinaryNode(2, BinaryNode(4), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).hasSymmetricStructure() should be(true)
    }

    it ("should return true for asymmetric trees") {
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, BinaryNode(3), Leaf)).hasSymmetricStructure() should be(false)
      BinaryNode(1, BinaryNode(2, BinaryNode(4), BinaryNode(2, BinaryNode(4), Leaf)), BinaryNode(2, BinaryNode(2, BinaryNode(4), Leaf), BinaryNode(3))).hasSymmetricStructure() should be(false)
    }
  }

  describe("symmetricBalancedTrees") {
    it ("should return all and only the symmetric complete balanced trees with n nodes") {
      BinaryTree.symmetricBalancedTrees(2, "x") should equal(Nil)

      BinaryTree.symmetricBalancedTrees(3, "x") should equal(Seq(new BinaryNode("x", BinaryNode("x"), BinaryNode("x"))))

      BinaryTree.symmetricBalancedTrees(4, "x") should equal(Nil)
      BinaryTree.symmetricBalancedTrees(5, "x") should equal(Seq(
        new BinaryNode("x", new BinaryNode("x", BinaryNode("x"), Leaf), new BinaryNode("x", Leaf, BinaryNode("x"))),
        new BinaryNode("x", new BinaryNode("x", Leaf, BinaryNode("x")), new BinaryNode("x", BinaryNode("x"), Leaf))))
      BinaryTree.symmetricBalancedTrees(6, "x") should equal(Nil)
    }
  }

  describe("hBalanced") {
    it ("should return a Leaf for height 0") {
      BinaryTree.hBalanced(0, "A") should equal(Seq(Leaf))
      BinaryTree.hBalanced(0, 1) should equal(Seq(Leaf))
    }

    it ("should return a singleton for height 1") {
      BinaryTree.hBalanced(1, "A") should equal(Seq(BinaryNode("A")))
      BinaryTree.hBalanced(1, 1) should equal(Seq(BinaryNode(1)))
    }

    it ("should return 3 trees with height 2, for h == 2") {
      BinaryTree.hBalanced(2, "A") should equal(Seq(
        new BinaryNode("A", BinaryNode("A"), BinaryNode("A")),
        new BinaryNode("A", Leaf, BinaryNode("A")),
        new BinaryNode("A", BinaryNode("A"), Leaf)
      ))
    }

    it ("should return 9 trees with height 3, for h == 3") {
      BinaryTree.hBalanced(3, "A") should equal(Seq(
        new BinaryNode("A", new BinaryNode("A", BinaryNode("A"), BinaryNode("A")), new BinaryNode("A", BinaryNode("A"), BinaryNode("A"))),
        new BinaryNode("A", new BinaryNode("A", BinaryNode("A"), BinaryNode("A")), BinaryNode("A")),
        new BinaryNode("A", BinaryNode("A"), new BinaryNode("A", BinaryNode("A"), BinaryNode("A"))),
        new BinaryNode("A", new BinaryNode("A", Leaf, BinaryNode("A")), new BinaryNode("A", Leaf, BinaryNode("A"))),
        new BinaryNode("A", new BinaryNode("A", Leaf, BinaryNode("A")), BinaryNode("A")),
        new BinaryNode("A", BinaryNode("A"), new BinaryNode("A", Leaf, BinaryNode("A"))),
        new BinaryNode("A", new BinaryNode("A", BinaryNode("A"), Leaf), new BinaryNode("A", BinaryNode("A"), Leaf)),
        new BinaryNode("A", new BinaryNode("A", BinaryNode("A"), Leaf), BinaryNode("A")),
        new BinaryNode("A", BinaryNode("A"), new BinaryNode("A", BinaryNode("A"), Leaf))
      ))
    }

    it ("should return 63 trees for height 4") {
      BinaryTree.hBalanced(4, "A").size should be(63)
    }
  }

  describe("minHbalNodes") {
    it ("should throw IllegalArgumentException for negative Int") {
      a[IllegalArgumentException] should be thrownBy {
        BinaryTree.minHbalNodes(-1)
      }
    }

    it ("should return the correct value for base cases") {
      BinaryTree.minHbalNodes(0) should be(0)
      BinaryTree.minHbalNodes(1) should be(1)
    }

    it ("should return the correct value for inductive cases") {
      BinaryTree.minHbalNodes(2) should be(2)
      BinaryTree.minHbalNodes(3) should be(4)
      BinaryTree.minHbalNodes(4) should be(7)
      BinaryTree.minHbalNodes(5) should be(12)
    }
  }

  describe("maxHbalHeightSlow") {
    it ("should return the correct value for inductive cases") {
      BinaryTree.maxHbalHeightSlow(1) should be(1)
      BinaryTree.maxHbalHeightSlow(2) should be(2)
      BinaryTree.maxHbalHeightSlow(3) should be(2)
      BinaryTree.maxHbalHeightSlow(4) should be(3)
      BinaryTree.maxHbalHeightSlow(5) should be(3)
      BinaryTree.maxHbalHeightSlow(6) should be(3)
      BinaryTree.maxHbalHeightSlow(7) should be(4)
      BinaryTree.maxHbalHeightSlow(8) should be(4)
      BinaryTree.maxHbalHeightSlow(9) should be(4)
      BinaryTree.maxHbalHeightSlow(10) should be(4)
      BinaryTree.maxHbalHeightSlow(11) should be(4)
      BinaryTree.maxHbalHeightSlow(12) should be(5)
    }
  }

  describe("maxHbalHeight") {
    it("should throw IllegalArgumentException for negative Int") {
      a[IllegalArgumentException] should be thrownBy {
        BinaryTree.maxHbalHeight(-1)
      }
    }

    it("should return the correct value for base cases") {
      BinaryTree.maxHbalHeight(0) should be(0)
      BinaryTree.maxHbalHeight(1) should be(1)
    }

    it("should return the correct value for inductive cases") {
      BinaryTree.maxHbalHeight(2) should be(2)
      BinaryTree.maxHbalHeight(3) should be(2)
      BinaryTree.maxHbalHeight(4) should be(3)
      BinaryTree.maxHbalHeight(5) should be(3)
      BinaryTree.maxHbalHeight(6) should be(3)
      BinaryTree.maxHbalHeight(7) should be(4)
      BinaryTree.maxHbalHeight(8) should be(4)
      BinaryTree.maxHbalHeight(9) should be(4)
      BinaryTree.maxHbalHeight(10) should be(4)
      BinaryTree.maxHbalHeight(11) should be(4)
      BinaryTree.maxHbalHeight(12) should be(5)
    }

    it ("should match the slow version [random test]") {
      (1 to 100) foreach { _ =>
        val n = 10 + Random.nextInt(1000)
        BinaryTree.maxHbalHeight(n) should be(BinaryTree.maxHbalHeightSlow(n))
      }
    }
  }

}
