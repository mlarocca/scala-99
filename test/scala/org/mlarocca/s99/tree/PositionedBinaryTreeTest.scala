package org.mlarocca.s99.tree

import org.scalatest._

import scala.util.Random

class PositionedBinaryTreeTest extends FunSpec with Matchers {

  describe("equality") {
    it("should compare correctly to empty trees") {
      PositionedBinaryLeaf should equal(PositionedBinaryLeaf)
      PositionedBinaryNode(1, 1, 1) should not equal (PositionedBinaryLeaf)
    }

    it("should return true for two singleton nodes with the same key and same position") {
      PositionedBinaryNode(1, 1, 1) should equal(PositionedBinaryNode(1, 1, 1))
      PositionedBinaryNode(true, 0, 0) should equal(PositionedBinaryNode(true, 0, 0))
      PositionedBinaryNode(false, 1, 4) should equal(PositionedBinaryNode(false, 1, 4))
      PositionedBinaryNode("abcd", 2, 3) should equal(PositionedBinaryNode("abcd", 2, 3))
    }

    it("should return false for two singleton nodes with the same key but different position") {
      PositionedBinaryNode(1, 2, 1) should not equal (PositionedBinaryNode(1, 1, 1))
      PositionedBinaryNode(true, 0, 4) should not equal (PositionedBinaryNode(true, 0, 0))
      PositionedBinaryNode(false, 0, 4) should not equal (PositionedBinaryNode(false, 1, 4))
      PositionedBinaryNode("abcd", 2, 1) should not equal (PositionedBinaryNode("abcd", 2, 3))
    }

    it("should return false for two roots with different keys, independently of the position") {
      PositionedBinaryNode(Random.nextInt(), 1, 3) should not equal (PositionedBinaryNode(-1, 1, 3))
      PositionedBinaryNode(Random.nextInt(), 0, 3) should not equal (PositionedBinaryNode(-1, 1, 3))
      PositionedBinaryNode(1 + Random.nextInt(), 0, 0) should not equal (PositionedBinaryNode(0, 0, 0))
      PositionedBinaryNode(1 + Random.nextInt(), 1, 0) should not equal (PositionedBinaryNode(0, 0, 0))
      PositionedBinaryNode(1, 1, 2) should not equal (PositionedBinaryNode(2, 2, 1))
      PositionedBinaryNode(1, 2, 1) should not equal (PositionedBinaryNode(2, 2, 1))
      PositionedBinaryNode("1", 0, 0) should not equal (PositionedBinaryNode("2", 0, 0))
      PositionedBinaryNode("abc", 1, 2) should not equal (PositionedBinaryNode("abcd", 3, 4))
      PositionedBinaryNode("abcd", 1, 2) should not equal (PositionedBinaryNode("abc", 1, 2))
      PositionedBinaryNode(true, 1, 5) should not equal (PositionedBinaryNode(false, 9, 4))
      PositionedBinaryNode(true, -1, 1) should not equal (PositionedBinaryNode(1, -1, 1))
    }

    it("should return false for trees with branches switched") {
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1) should not equal (new PositionedBinaryNode(2, PositionedBinaryNode(3, 3, 2), PositionedBinaryNode(1, 1, 2), None, 2, 1))
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1) should not equal (new PositionedBinaryNode(1, PositionedBinaryNode(2, 2, 1), PositionedBinaryNode(3, 3, 2), None, 1, 2))
    }

    it("should compare correctly deeper trees") {
      new PositionedBinaryNode(1, PositionedBinaryNode(2, 1, 2), PositionedBinaryNode(3, 1, 2), None, 1, 1) should equal(new PositionedBinaryNode(1, PositionedBinaryNode(2, 1, 2), PositionedBinaryNode(3, 1, 2), None, 1, 1))
      new PositionedBinaryNode(1, PositionedBinaryLeaf, PositionedBinaryLeaf, None, 1, 1) should equal(PositionedBinaryNode(1, 1, 1))

      new PositionedBinaryNode(1, PositionedBinaryNode(2, 2, 2), PositionedBinaryLeaf, Some(false), 1, 1) should not equal (new PositionedBinaryNode(1, PositionedBinaryLeaf, PositionedBinaryNode(2, 2, 2), Some(false), 1, 1))

      new PositionedBinaryNode(1, PositionedBinaryNode(2, 2, 2), PositionedBinaryLeaf, Some(false), 1, 1) should not equal (new PositionedBinaryNode(1, PositionedBinaryNode(2, 2, 2), PositionedBinaryNode(3, 3, 3), Some(false), 1, 1))
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1) should not equal (new PositionedBinaryNode(1, PositionedBinaryNode(2, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1))
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1) should not equal (new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 4), PositionedBinaryNode(3, 3, 4), None, 2, 1))

      new PositionedBinaryNode(1, PositionedBinaryLeaf, PositionedBinaryLeaf, Some('x'), 1, 1) should not equal (new PositionedBinaryNode(1, PositionedBinaryLeaf, PositionedBinaryNode(3, 3, 3), Some('x'), 1, 1))

      new PositionedBinaryNode("x", new PositionedBinaryNode("x", PositionedBinaryLeaf, PositionedBinaryNode("x", 2, 1), Some(true), 1, 2), new PositionedBinaryNode("x", PositionedBinaryNode("x", 3, 3), PositionedBinaryLeaf, None, 5, 5), None, 4, 2) should not
      equal(new PositionedBinaryNode("x", new PositionedBinaryNode("x", PositionedBinaryLeaf, PositionedBinaryNode("x", 2, 1), Some(true), 1, 2), new PositionedBinaryNode("x", PositionedBinaryLeaf, PositionedBinaryNode("x", 3, 3), None, 5, 5), None, 4, 2))
    }
  }

  describe("leftMostX") {
    it("should return 0 for Leaves") {
      PositionedBinaryLeaf.leftMostX() should be(0)
    }

    it ("should return x position for singleton nodes") {
      PositionedBinaryNode(1, 1, 1).leftMostX() should be(1)
      PositionedBinaryNode(true, 0, 0).leftMostX() should be(0)
      PositionedBinaryNode(false, 1, 4).leftMostX() should be(1)
      PositionedBinaryNode("abcd", 2, 3).leftMostX() should be(2)
    }

    it ("should return x attribute of the rightmost node (indepentently of current values)") {
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1).leftMostX() should be(1)
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 3, 2), PositionedBinaryNode(3, 1, 2), None, 2, 1).leftMostX() should be(3)
    }
  }

  describe("rightMostX") {
    it("should return 0 for Leaves") {
      PositionedBinaryLeaf.rightMostX() should be(0)
    }

    it ("should return x position for singleton nodes") {
      PositionedBinaryNode(1, 1, 1).rightMostX() should be(1)
      PositionedBinaryNode(true, 0, 0).rightMostX() should be(0)
      PositionedBinaryNode(false, 1, 4).rightMostX() should be(1)
      PositionedBinaryNode("abcd", 2, 3).rightMostX() should be(2)
    }

    it ("should return x attribute of the rightmost node (indepentently of current values)") {
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 1, 2), PositionedBinaryNode(3, 3, 2), None, 2, 1).rightMostX() should be(3)
      new PositionedBinaryNode(2, PositionedBinaryNode(1, 3, 2), PositionedBinaryNode(3, 1, 2), None, 2, 1).rightMostX() should be(1)
    }
  }

  describe("bounds") {
    it("should return the right bounds for a singleton") {
      PositionedBinaryNode("A", "2", 1, 1).bounds should be(Map(1 -> Bound(1, 1)))
    }

    it("should return the right bounds for a simple Tree") {
      val tree1 = new PositionedBinaryNode("a",
        PositionedBinaryLeaf,
        new PositionedBinaryNode("b",
          PositionedBinaryLeaf,
          PositionedBinaryNode("c", 3, 3),
          None, 2, 2),
        None, 1, 1)

      tree1.bounds should be(Map(
          1 -> Bound(1, 3),
          2 -> Bound(2, 4),
          3 -> Bound(3, 3)
      ))

      tree1.right.bounds should be(Map(
        1 -> Bound(2, 4),
        2 -> Bound(3, 3)
      ))

      val tree2 = new PositionedBinaryNode("b",
        PositionedBinaryNode("a", 1, 2),
        PositionedBinaryNode("c", 3, 2),
      None, 2, 1)

      tree2.left.bounds should be(Map(1 -> Bound(1, 1)))
      tree2.right.bounds should be(Map(1 -> Bound(3, 3)))
      tree2.bounds should be(
        Map(
          1 -> Bound(0, 4),
          2 -> Bound(1, 3)
      ))

      val tree3 = new PositionedBinaryNode("c",
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 3),
          PositionedBinaryLeaf,
          None, 2, 2),
        PositionedBinaryLeaf,
        None, 3, 1)

      tree3.left.bounds should be(Map(
        1 -> Bound(0, 2),
        2 -> Bound(1, 1)
      ))
      tree3.bounds should be(
        Map(
          1 -> Bound(1, 3),
          2 -> Bound(0, 2),
          3 -> Bound(1, 1)
        ))
    }

    it("should layout the tree correctly for larger trees") {
      val tree1 = new PositionedBinaryNode("a",
        PositionedBinaryLeaf,
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 3),
          PositionedBinaryNode("c", 3, 3),
          None, 2, 2),
        None, 1, 1)

      tree1.bounds should be(Map(
        1 -> Bound(1, 3),
        2 -> Bound(0, 4),
        3 -> Bound(1, 3)
      ))

      val tree2 = new PositionedBinaryNode("5",
        new PositionedBinaryNode("2",
          new PositionedBinaryNode("1",
            PositionedBinaryLeaf,
            PositionedBinaryNode("3", 3, 4),
            None, 1, 3),
          PositionedBinaryNode("4", 4, 3),
          None, 2, 2),
        new PositionedBinaryNode("7",
          new PositionedBinaryNode("6",
            PositionedBinaryNode("5.5", 5, 4),
            PositionedBinaryNode("6.5", 7, 4),
            None, 6, 3),
          PositionedBinaryNode("8", 8, 3),
          None, 7, 2),
        None, 5, 1)

      tree2.bounds should be(Map(
        1 -> Bound(1, 8),
        2 -> Bound(0, 9),
        3 -> Bound(1, 8),
        4 -> Bound(3, 7)
      ))
    }
  }

  describe("compactTree") {
    describe("should return the right compact() for a singleton") {
      it("should not move left aligned nodes") {
        PositionedBinaryNode("A", "2", 1, 1).compactTree(true) should be(PositionedBinaryNode("A", "2", 1, 1))
        PositionedBinaryNode("A", "2", 1, 1).compactTree(false) should be(PositionedBinaryNode("A", "2", 1, 1))
      }
      it("should not move nodes if `isRoot`param is false") {
        PositionedBinaryNode("A", "2", 2, 1).compactTree(false) should be(PositionedBinaryNode("A", "2", 2, 1))
        PositionedBinaryNode("A", "2", 3, 1).compactTree(false) should be(PositionedBinaryNode("A", "2", 3, 1))
      }
      it("should align nodes if `isRoot`param is true") {
        PositionedBinaryNode("A", "2", 2, 1).compactTree(true) should be(PositionedBinaryNode("A", "2", 1, 1))
        PositionedBinaryNode("A", "2", 3, 1).compactTree(true) should be(PositionedBinaryNode("A", "2", 1, 1))
      }
    }

    it("should return the right compact() for a simple Tree") {
      val tree1 = new PositionedBinaryNode("a",
        PositionedBinaryLeaf,
        new PositionedBinaryNode("b",
          PositionedBinaryLeaf,
          PositionedBinaryNode("c", 3, 3),
          None, 2, 2),
        None, 1, 1)

      val tree1A = new PositionedBinaryNode("a",
        PositionedBinaryLeaf,
        new PositionedBinaryNode("b",
          PositionedBinaryLeaf,
          PositionedBinaryNode("c", 4, 3),
          None, 2, 2),
        None, 1, 1)

      val tree1B = new PositionedBinaryNode("a",
        PositionedBinaryLeaf,
        new PositionedBinaryNode("b",
          PositionedBinaryLeaf,
          PositionedBinaryNode("c", 6, 3),
          None, 3, 2),
        None, 1, 1)

      tree1A.compactTree(true) should equal(tree1)
      tree1B.compactTree(true) should equal(tree1)

      val tree2 = new PositionedBinaryNode("b",
        PositionedBinaryNode("a", 1, 2),
        PositionedBinaryNode("c", 3, 2),
        None, 2, 1)

      val tree2A = new PositionedBinaryNode("b",
        PositionedBinaryNode("a", 2, 2),
        PositionedBinaryNode("c", 6, 2),
      None, 3, 1)

      val tree2B = new PositionedBinaryNode("b",
        PositionedBinaryNode("a", 2, 2),
        PositionedBinaryNode("c", 6, 2),
        None, 5, 1)

      tree2A.compactTree(true) should be(tree2)
      tree2B.compactTree(true) should be(tree2)

      val tree3 = new PositionedBinaryNode("c",
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 3),
          PositionedBinaryLeaf,
          None, 2, 2),
        PositionedBinaryLeaf,
        None, 3, 1)

      val tree3A = new PositionedBinaryNode("c",
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 3),
          PositionedBinaryLeaf,
          None, 3, 2),
        PositionedBinaryLeaf,
        None, 6, 1)

      tree3A.compactTree(true) should be(tree3)
    }

    it("should layout the tree correctly for larger trees") {
    /**
      val tree1 = new PositionedBinaryNode("a",
        PositionedBinaryLeaf,
        new PositionedBinaryNode("b",
          PositionedBinaryNode("a", 1, 3),
          PositionedBinaryNode("c", 3, 3),
          None, 2, 2),
        None, 1, 1)

      tree1.compact() should be(Map(
        1 -> Bound(1, 3),
        2 -> Bound(0, 4),
        3 -> Bound(1, 3)
      ))

      val tree2 = new PositionedBinaryNode("5",
        new PositionedBinaryNode("2",
          new PositionedBinaryNode("1",
            PositionedBinaryLeaf,
            PositionedBinaryNode("3", 3, 4),
            None, 1, 3),
          PositionedBinaryNode("4", 4, 3),
          None, 2, 2),
        new PositionedBinaryNode("7",
          new PositionedBinaryNode("6",
            PositionedBinaryNode("5.5", 5, 4),
            PositionedBinaryNode("6.5", 7, 4),
            None, 6, 3),
          PositionedBinaryNode("8", 8, 3),
          None, 7, 2),
        None, 5, 1)

      tree2.compact() should be(Map(
        1 -> Bound(1, 8),
        2 -> Bound(0, 9),
        3 -> Bound(1, 8),
        4 -> Bound(3, 7)
      ))
      */
    }
  }
}