package org.mlarocca.s99.tree


import org.scalatest._

import scala.util.Random

class BinarySearchTreeTest extends FunSpec with Matchers {

  describe("equality") {
    it("should compare correctly to empty binary search trees") {
      BinarySearchLeaf should equal(BinarySearchLeaf)
      BinarySearchNode(1) should not equal(BinarySearchLeaf)
    }

    it("should not equal Leaf") {
      BinarySearchLeaf should not equal(Leaf)
    }

    it ("should return true for two roots with the same key") {
      BinarySearchNode(1) should equal(BinarySearchNode(1))
      BinarySearchNode(true) should equal(BinarySearchNode(true))
      BinarySearchNode(false) should equal(BinarySearchNode(false))
      BinarySearchNode("abcd") should equal(BinarySearchNode("abcd"))
    }

    it ("should return true for two roots with the same key, despite the type  and value of `value`") {
      BinarySearchNode[Int, String](1, Some("false")) should equal(BinarySearchNode[Int, Boolean](1, Some(false)))
      BinarySearchNode(true, Some(1)) should equal(BinarySearchNode(true, None))
    }

    it ("should return false for binary trees roots with the same key") {
      BinarySearchNode(1) should equal(BinaryNode(1))
      BinarySearchNode(true) should equal(BinaryNode(true))
      BinarySearchNode(false) should equal(BinaryNode(false))
      BinarySearchNode("abcd") should equal(BinaryNode("abcd"))
    }

    it ("should return false for two roots with different keys") {
      BinarySearchNode(Random.nextInt()) should not equal (BinarySearchNode(-1))
      BinarySearchNode(1 + Random.nextInt()) should not equal (BinarySearchNode(0))
      BinarySearchNode(1) should not equal(BinarySearchNode(2))
      BinarySearchNode("1") should not equal(BinarySearchNode("2"))
      BinarySearchNode("abc") should not equal(BinarySearchNode("abcd"))
      BinarySearchNode("abcd") should not equal(BinarySearchNode("abc"))
      BinarySearchNode(true) should not equal(BinarySearchNode(false))
      BinarySearchNode(true) should not equal(BinarySearchNode(1))
    }

    it("should compare correctly deeper trees") {
      new BinarySearchNode(1, BinarySearchNode(2), BinarySearchNode(3)) should equal(new BinarySearchNode(1, BinarySearchNode(2), BinarySearchNode(3)))
      new BinarySearchNode(1, BinarySearchLeaf, BinarySearchLeaf) should equal(BinarySearchNode(1))

      new BinarySearchNode(1, BinarySearchNode(2), BinarySearchLeaf) should not equal(new BinarySearchNode(1, BinarySearchNode(2), BinarySearchNode(3)))
      new BinarySearchNode(2, BinarySearchNode(1), BinarySearchNode(3)) should not equal(new BinarySearchNode(1, BinarySearchNode(2), BinarySearchNode(3)))
      new BinarySearchNode(1, BinarySearchNode(3), BinarySearchNode(2)) should not equal(new BinarySearchNode(1, BinarySearchNode(2), BinarySearchNode(3)))
      new BinarySearchNode(1, BinarySearchLeaf, BinarySearchLeaf) should not equal(new BinarySearchNode(1, BinarySearchLeaf, BinarySearchNode(3)))
    }
  }

  describe("insert") {
    it("should insert correctly a key in an empty tree") {
      BinarySearchLeaf.insert(1, None) should equal(BinarySearchNode[Int, Nothing](1, None))
    }

    it("should insert correctly a key in a tree") {
      BinarySearchNode[Int, Nothing](2, None).insert(1, None) should equal(new BinarySearchNode[Int, Nothing](2, BinarySearchNode[Int, Nothing](1, None), BinarySearchLeaf, None))
      BinarySearchNode[Int, Nothing](2, None).insert(1, None).insert(3, None) should equal(new BinarySearchNode[Int, Nothing](2, BinarySearchNode[Int, Nothing](1, None), BinarySearchNode[Int, Nothing](3, None), None))
    }
    it("should insert correctly a key in a deep tree") {
      BinarySearchNode[Int, Nothing](2, None).insert(1, None).insert(3, None).insert(4, Some("4")) should
          equal(new BinarySearchNode[Int, Boolean](2, BinarySearchNode[Int, Boolean](1, None), new BinarySearchNode[Int, Boolean](3, BinarySearchLeaf, BinarySearchNode[Int, Boolean](4, Some(false)), None), None))
    }
  }
}
