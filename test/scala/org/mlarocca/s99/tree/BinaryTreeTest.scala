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

  describe("size") {
    it ("should be 0 for a leaf") {
      Leaf.size should be(0)
    }

    it ("should be 1 for a single node") {
      BinaryNode(null).size should be(1)
    }

    it ("should be compute correctly for larger trees") {
      BinaryNode(1, BinaryNode(2), BinaryNode(2)).size() should be(3)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).size() should be(5)
      BinaryNode(1,
        BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)),
        BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(5)), BinaryNode(3))).size() should be(9)
    }
  }

  describe("height") {
    it ("should be 0 for a leaf") {
      Leaf.height should be(0)
    }

    it ("should be 1 for a single node") {
      BinaryNode(null).height should be(1)
    }

    it ("should be compute correctly for larger trees") {
      BinaryNode(1, BinaryNode(2), BinaryNode(2)).height() should be(2)
      BinaryNode(1, Leaf, BinaryNode(2)).height() should be(2)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).height() should be(3)
      BinaryNode(1,
        BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)),
        BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(5)), BinaryNode(3))).height() should be(4)
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

  describe("hBalancedWithNodes") {
    it ("should return a Leaf for n == 0") {
      BinaryTree.hBalancedWithNodes(0, "A") should equal(Seq(Leaf))
      BinaryTree.hBalancedWithNodes(0, 1) should equal(Seq(Leaf))
    }

    it ("should return a singleton for n == 1") {
      BinaryTree.hBalancedWithNodes(1, "A") should equal(Seq(BinaryNode("A")))
      BinaryTree.hBalancedWithNodes(1, 1) should equal(Seq(BinaryNode(1)))
    }

    it ("should return 2 trees with height 2, for n == 2") {
      BinaryTree.hBalancedWithNodes(2, "A") should equal(Seq(
        new BinaryNode("A", Leaf, BinaryNode("A")),
        new BinaryNode("A", BinaryNode("A"), Leaf)
      ))
    }

    it ("should return 1 trees with height 2, for n == 3") {
      BinaryTree.hBalancedWithNodes(3, "A") should equal(Seq(
        new BinaryNode("A", BinaryNode("A"), BinaryNode("A"))
      ))
    }

    it ("should return 4 trees with height 3, for n == 4") {
      BinaryTree.hBalancedWithNodes(4, "A") should equal(Seq(
        new BinaryNode("A", new BinaryNode("A", Leaf, BinaryNode("A")), BinaryNode("A")),
        new BinaryNode("A", new BinaryNode("A", BinaryNode("A"), Leaf), BinaryNode("A")),
        new BinaryNode("A", BinaryNode("A"), new BinaryNode("A", Leaf, BinaryNode("A"))),
        new BinaryNode("A", BinaryNode("A"), new BinaryNode("A", BinaryNode("A"), Leaf))
      ))
    }

    it ("should return 63 trees for n == 15") {
      BinaryTree.hBalancedWithNodes(15, "B").size should be(177)
    }
  }
  
  describe("leafCount") {
    it ("should be 1 for a leaf") {
      Leaf.leafNodeCount should be(0)
    }

    it ("should be 2 for a single node") {
      BinaryNode(null).leafNodeCount should be(1)
    }

    it ("should be compute correctly for larger trees") {
      BinaryNode(1, BinaryNode(2), BinaryNode(2)).leafNodeCount() should be(2)
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).leafNodeCount() should be(2)
      BinaryNode(1,
        BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)),
        BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(5)), BinaryNode(3))).leafNodeCount() should be(4)
    }
  }

  describe("leafNodeSeq") {
    it ("should be Nil for a leaf") {
      Leaf.leafNodeSeq should be(Nil)
    }

    it ("should be a singleton for a single node") {
      BinaryNode(null).leafNodeSeq should be(Seq((null, None)))
      new BinaryNode(1, Leaf, Leaf, Some("f")).leafNodeSeq should be(Seq((1, Some("f"))))
    }

    it ("should be compute correctly for larger trees") {
      BinaryNode(1, BinaryNode(2), BinaryNode(2, "x")).leafNodeSeq() should be(Seq((2, None), (2, Some("x"))))
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3))).leafNodeSeq() should be(Seq((3, None), (3, None)))
      BinaryNode(1,
        BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)),
        BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(7)), BinaryNode(8))).leafNodeSeq() should be(Seq(3, 5, 7, 8).map((_, None)))
    }
  }

  describe("internalNodeSeq") {
    it ("should be 1 for a leaf") {
      Leaf.internalNodeSeq should be(Nil)
    }

    it ("should be Nil for a single node") {
      BinaryNode(null).internalNodeSeq should be(Nil)
      new BinaryNode(1, Leaf, Leaf, Some("f")).internalNodeSeq should be(Nil)
    }

    it ("should be compute correctly for larger trees") {
      BinaryNode(1, BinaryNode(2), BinaryNode(2, Some("x"))).internalNodeSeq() should be(Seq((1, None)))
      BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3), Some("x")), Some("y")).internalNodeSeq() should
          be(Seq((1, Some("y")), (2, None), (2, Some("x"))))
      BinaryNode(1,
        BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)),
        BinaryNode(2, BinaryNode(4, Leaf, BinaryNode(7)), BinaryNode(8))).internalNodeSeq() should
          be(Seq(1, 2, 4, 2, 4).map((_, None)))
    }
  }

  describe("nodesAtLevel") {
    it ("should throw IllegalArgumentException for negative Int") {
      a[IllegalArgumentException] should be thrownBy {
        Leaf.nodesAtLevel(-2)
      }
      a[IllegalArgumentException] should be thrownBy {
        BinaryNode(123).nodesAtLevel(-1)
      }
    }

    it ("should throw IllegalArgumentException when 0 is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Leaf.nodesAtLevel(0)
      }
      a[IllegalArgumentException] should be thrownBy {
        BinaryNode(1, BinaryNode(2), Leaf).nodesAtLevel(-1)
      }
    }

    it ("should be the root for level 1") {
      BinaryNode(1, false).nodesAtLevel(1) should be(Seq((1, Some(false))))
    }

    it ("should be computed correctly for larger trees") {
      val t1 = BinaryNode(1, BinaryNode(2), BinaryNode(2, "x"))
      t1.nodesAtLevel(1) should be(Seq((1, None)))
      t1.nodesAtLevel(2) should be(Seq((2, None), (2, Some("x"))))

      val t2 = BinaryNode(1, BinaryNode(2, BinaryNode(3), Leaf), BinaryNode(2, Leaf, BinaryNode(3), Some("x")), Some("y"))

      t2.nodesAtLevel(1) should be(Seq((1, Some("y"))))
      t2.nodesAtLevel(2) should be(Seq((2, None), (2, Some("x"))))
      t2.nodesAtLevel(3) should be(Seq((3, None), (3, None)))

      val t3 = BinaryNode(1,
        BinaryNode(2, BinaryNode(3), BinaryNode(4, BinaryNode(5), Leaf)),
        BinaryNode(2, BinaryNode(6, Leaf, BinaryNode(7)), BinaryNode(8)))

      t3.nodesAtLevel(1) should be(Seq((1, None)))
      t3.nodesAtLevel(2) should be(Seq((2, None), (2, None)))
      t3.nodesAtLevel(3) should be(Seq((3, None), (4, None), (6, None), (8, None)))
      t3.nodesAtLevel(4) should be(Seq((5, None), (7, None)))
      t3.nodesAtLevel(5) should be(Nil)
    }
  }

  describe("completeBinaryTree") {
    it ("should throw IllegalArgumentException for negative Int") {
      a[IllegalArgumentException] should be thrownBy {
        BinaryTree.completeBinaryTree(-2, "1")
      }
      a[IllegalArgumentException] should be thrownBy {
        BinaryTree.completeBinaryTree(-1, Random.nextBoolean())
      }
    }

    it ("should return a Leaf for n == 0") {
      BinaryTree.completeBinaryTree(0, Random.nextDouble()) should be(Leaf)
    }

    it ("should return a singleton for n == 1") {
      BinaryTree.completeBinaryTree(1, "x") should be(BinaryNode("x"))
    }

    it ("should be generated correctly for larger trees") {
      BinaryTree.completeBinaryTree(2, "A") should be(BinaryNode("A", BinaryNode("A"), Leaf))
      BinaryTree.completeBinaryTree(3, "A") should be(BinaryNode("A", BinaryNode("A"), BinaryNode("A")))

      BinaryTree.completeBinaryTree(4, "A") should
          be(BinaryNode("A", BinaryNode("A", BinaryNode("A"), Leaf), BinaryNode("A")))
      BinaryTree.completeBinaryTree(5, "A") should
          be(BinaryNode("A", BinaryNode("A", BinaryNode("A"), BinaryNode("A")), BinaryNode("A")))
      BinaryTree.completeBinaryTree(6, "A") should
          be(BinaryNode("A", BinaryNode("A", BinaryNode("A"), BinaryNode("A")), BinaryNode("A", BinaryNode("A"), Leaf)))
      BinaryTree.completeBinaryTree(7, "A") should
          be(BinaryNode("A", BinaryNode("A", BinaryNode("A"), BinaryNode("A")), BinaryNode("A", BinaryNode("A"), BinaryNode("A"))))
      BinaryTree.completeBinaryTree(8, "A") should
          be(BinaryNode("A", BinaryNode("A", BinaryNode("A", BinaryNode("A"), Leaf), BinaryNode("A")), BinaryNode("A", BinaryNode("A"), BinaryNode("A"))))
      BinaryTree.completeBinaryTree(9, "A") should
          be(BinaryNode("A", BinaryNode("A", BinaryNode("A", BinaryNode("A"), BinaryNode("A")), BinaryNode("A")), BinaryNode("A", BinaryNode("A"), BinaryNode("A"))))
    }
  }

  describe("layoutBinaryTree") {
    it ("should return a PositionedBinaryLeaf for a Leaf") {
      Leaf.layoutBinaryTree() should be(PositionedBinaryLeaf)
    }

    it ("should return a PositionedNode for a root") {
      BinaryNode("A", Some("2")).layoutBinaryTree() should be(PositionedBinaryNode("A", "2", 1, 1))
    }

    it ("should return the right PositionedTree for a simple Tree") {
      BinarySearchTree.fromKeySeq(Seq('a','b','c')).layoutBinaryTree() should be (
        new PositionedBinaryNode("a",
          PositionedBinaryLeaf,
          new PositionedBinaryNode("b",
            PositionedBinaryLeaf,
            PositionedBinaryNode("c", 3, 3), None, 2, 2),
          None, 1, 1))

      BinarySearchTree.fromKeySeq(Seq('b','a','c')).layoutBinaryTree() should be (
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 2),
          PositionedBinaryNode("c", 3, 2),
          None, 2, 1)
      )
      BinarySearchTree.fromKeySeq(Seq('c','b','a')).layoutBinaryTree() should be (
        new PositionedBinaryNode("c",
          new PositionedBinaryNode("b",
            PositionedBinaryNode("a", 1, 3),
            PositionedBinaryLeaf,
            None, 2, 2),
          PositionedBinaryLeaf,
          None, 3, 1))
    }

    it ("should layout the tree correctly for larger trees") {
      val layoutTree = BinarySearchTree.fromKeySeq(Seq('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTree()
      val inorder = layoutTree.toInOrderItemList()
      inorder.zipWithIndex.map {
        case (node, position) => node.x should be(position + 1) //Indices starts from 0
      }
      val h = layoutTree.height
      (1 to h).foreach { h =>
        layoutTree.itemsAtLevel(h).foreach { item: PositionedItem[Char, Nothing] =>
          item.y should be(h)
        }
      }
    }
  }

  describe("layoutBinaryTreeConstantSpace") {
    it ("should return a PositionedBinaryLeaf for a Leaf") {
      Leaf.layoutBinaryTreeConstantSpace() should be(PositionedBinaryLeaf)
    }

    it ("should return a PositionedNode for a root") {
      BinaryNode("A", Some("2")).layoutBinaryTreeConstantSpace() should be(PositionedBinaryNode("A", "2", 1, 1))
    }

    it ("should return the right PositionedTree for a simple Tree") {
      BinarySearchTree.fromKeySeq(Seq('a','b','c')).layoutBinaryTreeConstantSpace() should be (
        new PositionedBinaryNode("a",
          PositionedBinaryLeaf,
          new PositionedBinaryNode("b",
            PositionedBinaryLeaf,
            PositionedBinaryNode("c", 3, 3), None, 2, 2),
          None, 1, 1))

      BinarySearchTree.fromKeySeq(Seq('b','a','c')).layoutBinaryTreeConstantSpace() should be (
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 2),
          PositionedBinaryNode("c", 3, 2),
          None, 2, 1)
      )
      BinarySearchTree.fromKeySeq(Seq('c','b','a')).layoutBinaryTreeConstantSpace() should be (
        new PositionedBinaryNode("c",
          new PositionedBinaryNode("b",
            PositionedBinaryNode("a", 1, 3),
            PositionedBinaryLeaf,
            None, 2, 2),
          PositionedBinaryLeaf,
          None, 4, 1))
    }

    it ("should layout the tree correctly for larger trees") {
      val layoutTree = BinarySearchTree.fromKeySeq(Seq('n', 'k', 'c', 'm', 'a', 'e', 'd', 'g', 'u', 'p', 'q')).layoutBinaryTreeConstantSpace()
      layoutTree.itemsAtLevel(1).head.x should be(16)
      println(layoutTree)
      layoutTree.toString() should equal("T[16, 1](n T[8, 2](k T[2, 3](c T[1, 4](a . .) T[4, 4](e T[3, 5](d . .) T[5, 5](g . .))) T[9, 3](m . .)) T[20, 2](u T[17, 3](p . T[18, 4](q . .)) .))")
      val h = layoutTree.height
      (1 to h).foreach { h =>
        layoutTree.itemsAtLevel(h).foreach { item: PositionedItem[Char, Nothing] =>
          item.y should be(h)
        }
      }
    }

    it ("should layout the tree correctly for even larger trees") {
      val layoutTree = BinarySearchTree.fromKeySeq(Seq('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTreeConstantSpace()

      layoutTree.itemsAtLevel(1).head.x should be(32)
      layoutTree.toString() should equal("T[32, 1](n T[16, 2](k T[2, 3](c T[1, 4](a . .) T[6, 4](h T[4, 5](g T[3, 6](e . .) .) .)) T[17, 3](m . .)) T[40, 2](u T[33, 3](p . T[35, 4](s T[34, 5](q . .) .)) .))")
      val h = layoutTree.height
      (1 to h).foreach { h =>
        layoutTree.itemsAtLevel(h).foreach { item: PositionedItem[Char, Nothing] =>
          item.y should be(h)
        }
      }
    }
  }  
}
