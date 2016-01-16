package org.mlarocca.s99.tree

import org.scalatest._
import MultiWayTree.fromString
import scala.util.Random

class MultiwayTreeTest extends FunSpec with Matchers {

  describe("equality") {
    it ("should return true for two roots with the same key") {
      MultiWayTree(1) should equal(MultiWayTree(1))
      MultiWayTree(true) should equal(MultiWayTree(true))
      MultiWayTree(false) should equal(MultiWayTree(false))
      MultiWayTree("abcd") should equal(MultiWayTree("abcd"))
    }

    it ("should return match tree independently on the value") {
      MultiWayTree(1, None) should equal(MultiWayTree(1, Some(1)))
      MultiWayTree(true, Some('x')) should equal(MultiWayTree(true, Some(44)))
      MultiWayTree(false, Some(false)) should equal(MultiWayTree(false, Some('abc)))
      MultiWayTree("abcd", Some("efg")) should equal(MultiWayTree("abcd", Some("abcd")))
    }

    it ("should return false for two roots with different keys") {
      MultiWayTree(Random.nextInt()) should not equal (MultiWayTree(-1))
      MultiWayTree(1 + Random.nextInt()) should not equal (MultiWayTree(0))
      MultiWayTree(1) should not equal(MultiWayTree(2))
      MultiWayTree(1) should not equal(MultiWayTree('1'))
      MultiWayTree(1) should not equal(MultiWayTree("1"))
      MultiWayTree('1') should not equal(MultiWayTree("1"))
      MultiWayTree("1") should not equal(MultiWayTree("2"))
      MultiWayTree("abc") should not equal(MultiWayTree("abcd"))
      MultiWayTree("abcd") should not equal(MultiWayTree("abc"))
      MultiWayTree(true) should not equal(MultiWayTree(false))
      MultiWayTree(true) should not equal(MultiWayTree(1))
    }

    it("should compare correctly deeper trees") {
      MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3)) should equal(MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3)))
      MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3)) should not
        equal(MultiWayTree(1, None, MultiWayTree(2, None, MultiWayTree(3))))

      MultiWayTree(1, None, MultiWayTree(2)) should not equal(MultiWayTree(1))
      MultiWayTree(1, None, MultiWayTree(2)) should not equal(MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3)))
      MultiWayTree(2, None, MultiWayTree(1), MultiWayTree(3)) should not equal(MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3)))
      MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3), MultiWayTree('3')) should not equal(MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(3)))
    }
  }

  describe("size") {
    it ("should be 1 for a single node") {
      MultiWayTree(null).size should be(1)
    }

    it ("should be compute correctly for larger trees") {
      MultiWayTree(1, None, MultiWayTree(2)).size should be(2)
      MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(2)).size should be(3)
      MultiWayTree(1, None, MultiWayTree('2'), MultiWayTree('2'), MultiWayTree('2'), MultiWayTree(2)).size should be(5)
      MultiWayTree(1, None, MultiWayTree(2, None, MultiWayTree(3)), MultiWayTree(2, None, MultiWayTree(3))).size should be(5)
      MultiWayTree(1,
        Some("root"),
        MultiWayTree(2, None, MultiWayTree(3), MultiWayTree(4, None, MultiWayTree(5))),
        MultiWayTree(2, None, MultiWayTree(4, None, MultiWayTree(5)), MultiWayTree(3))).size should be(9)
    }
  }

  describe("height") {
    it ("should be 1 for a single node") {
      MultiWayTree(null).height should be(1)
    }

    MultiWayTree(1, None, MultiWayTree(2)).height should be(2)
    MultiWayTree(1, None, MultiWayTree(2), MultiWayTree(2)).height should be(2)
    MultiWayTree(1, None, MultiWayTree('2'), MultiWayTree('2'), MultiWayTree('2'), MultiWayTree(2)).height should be(2)
    MultiWayTree(1, None, MultiWayTree(2, None, MultiWayTree(3)), MultiWayTree(2, None, MultiWayTree(3))).height should be(3)
    MultiWayTree(1,
      Some("root"),
      MultiWayTree(2, None, MultiWayTree(3), MultiWayTree(4, None, MultiWayTree(5))),
      MultiWayTree(2, None, MultiWayTree(4, None, MultiWayTree(5)), MultiWayTree(3))).height should be(4)
  }

  describe("fromString") {
    it ("should throw IllegalArgumentException for malformed strings") {
      a[IllegalArgumentException] should be thrownBy {
        MultiWayTree.fromString("a")
      }

      a[IllegalArgumentException] should be thrownBy {
        MultiWayTree.fromString("ab^")
      }

      a[IllegalArgumentException] should be thrownBy {
        MultiWayTree.fromString("abc")
      }

      a[IllegalArgumentException] should be thrownBy {
        MultiWayTree.fromString("a^b^")
      }

      a[IllegalArgumentException] should be thrownBy {
        MultiWayTree.fromString("ab^^C")
      }
    }

    it ("should correctly parse simple (non-recursive) trees") {
      MultiWayTree.fromString("a^") should be(MultiWayTree('a'))
      MultiWayTree.fromString("ab^^") should be(MultiWayTree('a', None, MultiWayTree('b')))
      MultiWayTree.fromString("ab^c^^") should be(MultiWayTree('a', None, MultiWayTree('b'), MultiWayTree('c')))
      MultiWayTree.fromString("ab^c^d^e^^") should be(MultiWayTree('a', None, MultiWayTree('b'), MultiWayTree('c'), MultiWayTree('d'), MultiWayTree('e')))
    }

    it ("should correctly parse recursive trees") {
      MultiWayTree.fromString("abc^^^") should be(MultiWayTree('a', None, MultiWayTree('b', None, MultiWayTree('c'))))
      MultiWayTree.fromString("abc^d^^^") should be(MultiWayTree('a', None, MultiWayTree('b', None, MultiWayTree('c'), MultiWayTree('d'))))
      MultiWayTree.fromString("abc^d^^e^^") should be(MultiWayTree('a', None, MultiWayTree('b', None, MultiWayTree('c'), MultiWayTree('d')), MultiWayTree('e')))
      MultiWayTree.fromString("abc^de^^^fg^^^") should be(MultiWayTree('a', None, MultiWayTree('b', None, MultiWayTree('c'), MultiWayTree('d', None, MultiWayTree('e'))), MultiWayTree('f', None, MultiWayTree('g'))))
      MultiWayTree.fromString("abcde^f^^^^^") should be(MultiWayTree('a', None, MultiWayTree('b', None, MultiWayTree('c', None, MultiWayTree('d', None, MultiWayTree('e'), MultiWayTree('f'))))))
    }
  }

  describe("internalPathLength") {
    it ("should be 0 for a root with no children") {
      "a^".internalPathLength should be(0)
    }

    it ("should be the path length for a tree with 1 child") {
      "ab^^".internalPathLength should be(1)
    }

    it ("should be the sum of the path lengths for a linear tree") {
      "abc^^^".internalPathLength should be(3)
      "abcdef^^^^^^".internalPathLength should be{
        val n =  "abcdef^^^^^^".count(_ == '^')
        n * (n - 1) / 2
      }
    }

    it ("should be computed correctly for larger trees") {
      "afg^^c^bd^e^^^".internalPathLength should be(9)
    }
  }

  describe("preOrder") {
    it ("should be Seq(key) for a root without children") {
      "a^".preOrder() should be(Seq('a'))
      MultiWayTree("abc").preOrder() should be(Seq("abc"))
      MultiWayTree(1).preOrder() should be(Seq(1))
    }

    it ("should be computed for simple trees") {
      "ab^^".preOrder() should be(Seq('a', 'b'))
      MultiWayTree("abc", None, MultiWayTree("def")).preOrder() should be(Seq("abc", "def"))
      MultiWayTree(1, None, MultiWayTree(2)).preOrder() should be(Seq(1, 2))
    }
    it ("should be computed correctly for larger trees") {
      "afg^^c^bd^e^^^".preOrder() should be(Seq('a', 'f', 'g', 'c', 'b', 'd', 'e'))
    }
  }

  describe("postOrder") {
    it ("should be Seq(key) for a root without children") {
      "a^".postOrder() should be(Seq('a'))
      MultiWayTree("abc").postOrder() should be(Seq("abc"))
      MultiWayTree(1).postOrder() should be(Seq(1))
    }

    it ("should be computed for simple trees") {
      "ab^^".postOrder() should be(Seq('b', 'a'))
      MultiWayTree("abc", None, MultiWayTree("def")).postOrder() should be(Seq("def", "abc"))
      MultiWayTree(1, None, MultiWayTree(2)).postOrder() should be(Seq(2, 1))
    }
    it ("should be computed correctly for larger trees") {
      "afg^^c^bd^e^^^".postOrder() should be(Seq('g', 'f', 'c', 'd', 'e', 'b', 'a'))
    }
  }
}
