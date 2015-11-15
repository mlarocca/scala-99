package com.mlarocca.s99.lists

import org.scalatest._

class UtilsTest extends FunSpec with Matchers {

  describe("last") {
    it("should return the last value in a non empty sequence") {
      val s = Seq(1, 2, 3)
      val x = Utils.last(s)
      x.shouldBe(3)
      val y = Utils.last(Seq("1"))
      y.shouldBe("1")
    }

    it ("should throw NoSuchElementException if an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      a[NoSuchElementException] should be thrownBy {
        Utils.last(emptySeq)
      }
    }
  }

  describe("lastOption") {
    it ("should return the last value in a non empty sequence") {
      val s = Seq(1,2,3)
      val x = Utils.lastOption(s)
      x.shouldBe(Some(3))
      val y = Utils.lastOption(Seq("1"))
      y.shouldBe(Some("1"))
    }

    it ("should return None when an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      Utils.lastOption(emptySeq) should be(None)
    }
  }

  describe("penultimate") {
    it("should return the second to last value in a sequence with at least 2 elements") {
      val s = Seq(1, 2, 3)
      val x = Utils.penultimate(s)
      x.shouldBe(2)
      val y = Utils.penultimate(Seq("1", 2))
      y.shouldBe("1")
    }

    it ("should throw NoSuchElementException if an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      a[NoSuchElementException] should be thrownBy {
        Utils.penultimate(emptySeq)
      }
    }

    it ("should throw NoSuchElementException if a sequence with only 1 element is passed") {
      a[NoSuchElementException] should be thrownBy {
        Utils.penultimate(Seq(1))
      }
    }
  }

  describe("penultimateOption") {
    it ("should return the second to last value in a sequence with at least 2 elements") {
      val s = Seq(1,2,3)
      val x = Utils.penultimateOption(s)
      x.shouldBe(Some(2))
      val y = Utils.penultimateOption(Seq("1", "2"))
      y.shouldBe(Some("1"))
    }

    it ("should return None when an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      Utils.penultimateOption(emptySeq) should be(None)
    }

    it ("should return None when a sequence with a singol element is passed") {
      Utils.penultimateOption(Seq(false)) should be(None)
    }
  }

  describe("nth") {
    it("should return the nth value in a sequence with at least n+1 elements") {
      val s = Seq(1, 2, 3)
      Utils.nth(1, s) shouldBe(2)
      Utils.nth(0, Seq("1", 2)) shouldBe("1")
      Utils.nth(0, s) shouldBe(1)
      Utils.nth(2, s) shouldBe(3)
    }

    it ("should throw NoSuchElementException if an empty sequence is passed") {
      a[NoSuchElementException] should be thrownBy {
        Utils.nth(0, Seq.empty[String])
      }
    }


    it ("should throw IllegalArgumentException if a negative index is passed") {
    a[IllegalArgumentException] should be thrownBy {
      Utils.nth(-1, Seq(1, 2, 3))
    }

    a[IllegalArgumentException] should be thrownBy {
      Utils.nth(-1, Nil)
    }
  }

  it ("should throw NoSuchElementException if a sequence with too few elements is passed") {
    a[NoSuchElementException] should be thrownBy {
      Utils.nth(1, Seq(1))
    }

    a[NoSuchElementException] should be thrownBy {
      Utils.nth(2, Seq(1, 2))
    }

    a[NoSuchElementException] should be thrownBy {
      Utils.nth(3, Seq(1, 2))
    }
  }
}

  describe("nthOption") {
    it ("should return the nth value in a sequence with at least n+1 elements") {
      val s = Seq(1, 2, 3)
      val x = Utils.nthOption(1, s)
      x.shouldBe(Some(2))
      val y = Utils.nthOption(0, Seq("1", "2"))
      y.shouldBe(Some("1"))
    }

    it ("should throw IllegalArgumentException if a negative index is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.nthOption(-1, Seq(1,2,3))
      }

      a[IllegalArgumentException] should be thrownBy {
        Utils.nthOption(-1, Nil)
      }
    }

    it ("should return None if a sequence with too few elements is passed") {
      Utils.nthOption(1, Seq(1)) should be(None)
      Utils.nthOption(2, Seq(1,2)) should be(None)
      Utils.nthOption(3, Seq(1,2)) should be(None)
    }
  }

  describe("length") {
    it ("should be 0 for empty Seqs") {
      Utils.length(Nil) should be(0)
    }

    it ("should match Seq size") {
      Utils.length(Seq("a")) should be(1)
      Utils.length(Seq("a", 2)) should be(2)
      Utils.length(Seq(false, Utils.length _, 41)) should be(3)
    }
  }

  describe("reverse") {
    it ("reversing an empty Seq should return Nil") {
      Utils.reverse(Nil) should be(Nil)
    }

    it ("should correctly reverse Seq of any Type") {
      Utils.reverse(Seq("a")) should equal(Seq("a"))
      Utils.reverse(Seq("a", 2)) should equal(Seq(2, "a"))
      Utils.reverse(Seq(false, "true", 41)) should equal(Seq(41, "true", false))
    }
  }

  describe("isPalindrome") {
    it ("should return true for Empty sequences") {
      Utils.isPalindrome(Nil) should be(true)
    }

    it ("should return true for singletons") {
      Utils.isPalindrome(Seq("a")) should be(true)
      Utils.isPalindrome(Seq(2)) should be(true)
    }

    it ("should return true for palindrome sequences") {
      Utils.isPalindrome(Seq("a", "a")) should be(true)
      Utils.isPalindrome(Seq("a", 2, "a")) should be(true)
      Utils.isPalindrome(Seq("a", 2, 2, "a")) should be(true)
      Utils.isPalindrome(Seq("a", 2, false, 2, "a")) should be(true)
    }

    it ("should return false for non palindrome sequences") {
      Utils.isPalindrome(Seq("a", "b")) should be(right = false)
      Utils.isPalindrome(Seq("a", 2, 2)) should be(false)
      Utils.isPalindrome(Seq("a", 1, 2, "a")) should be(false)
      Utils.isPalindrome(Seq(true, 2, false, 2, "a")) should be(false)
    }
  }

  describe("flatten") {
    it ("should flatten Empty sequences into Nil") {
      Utils.flatten(Nil) should be(Nil)
    }

    it ("should flatten simple list returning the sequence itself") {
      Utils.flatten(Seq("a")) should be(Seq("a"))
      Utils.flatten(Seq("a", 2)) should be(Seq("a", 2))
      Utils.flatten(Seq(false, "a", 2)) should be(Seq(false, "a", 2))
    }
    it ("should flatten out empty nested lists") {
      Utils.flatten(Seq("a", Nil)) should be(Seq("a"))
      Utils.flatten(Seq(Nil, "a", 2)) should be(Seq("a", 2))
      Utils.flatten(Seq(false, Nil, "a", 2)) should be(Seq(false, "a", 2))
    }

    it ("should flatten second order nested lists returning the sequence of elements") {
      Utils.flatten(Seq("a", Seq("b", 2))) should be(Seq("a", "b", 2))
      Utils.flatten(Seq("a", Nil, 2)) should be(Seq("a", 2))
      Utils.flatten(Seq(Seq("b", 2, false), "a", Seq("b", 2))) should be(Seq("b", 2, false, "a", "b", 2))
    }

    it ("should flatten higher order nested lists returning the sequence of elements") {
      Utils.flatten(Seq("a", Seq(Seq("b", Nil, 2), 2))) should be(Seq("a", "b", 2, 2))
      Utils.flatten(Seq(Seq("a", Seq("b", Seq(Seq("c", Nil, 1), 2)), false, Seq("d", Seq(Seq("e", Nil, 3), 4))), "f", Seq("g", 5))) should be(Seq("a", "b", "c", 1, 2, false, "d", "e", 3, 4, "f", "g", 5))
    }
  }

  describe("compress") {
    it ("should compress Empty sequences into Nil") {
      Utils.compress(Nil) should be(Nil)
    }

    it ("should not change lists with no consecutive duplicate") {
      Utils.compress(Seq("a", "b", "c")) should be(Seq("a", "b", "c"))
      Utils.compress(Seq("a", "b", "a")) should be(Seq("a", "b", "a"))
    }

    it ("should remove consecutive duplicates") {
      Utils.compress(Seq("a", "b", "b")) should be(Seq("a", "b"))
      Utils.compress(Seq("a", "a", "a")) should be(Seq("a"))
      Utils.compress(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq("a", "b", "c", "d"))
      Utils.compress(Seq("a", "a", "b", "c", "c", "c", "d", "a", "a")) should be(Seq("a", "b", "c", "d", "a"))
    }
  }

  describe("pack") {
    it ("should pack Empty sequences into Nil") {
      Utils.pack(Nil) should be(Nil)
    }

    it ("should pack lists with no consecutive duplicate") {
      Utils.pack(Seq("a", "b", "c")) should be(Seq("a", "b", "c").map{Seq(_)})
      Utils.pack(Seq("a", "b", "a")) should be(Seq("a", "b", "a").map{Seq(_)})
    }

    it ("should pack consecutive duplicates") {
      Utils.pack(Seq("a", "b", "b")) should be(Seq(Seq("a"), Seq("b", "b")))
      Utils.pack(Seq("a", "a", "a")) should be(Seq(Seq("a", "a", "a")))
      Utils.pack(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(Seq("a", "a"), Seq("b"), Seq("c", "c", "c"), Seq("d")))
      Utils.pack(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq(Seq(1, 1), Seq(2), Seq(3, 3, 3), Seq(4), Seq(5, 5)))
    }
  }

  describe("encode") {
    it ("should encode Empty sequences into Nil") {
      Utils.encode(Nil) should be(Nil)
    }

    it ("should encode correctly lists with no consecutive duplicate") {
      Utils.encode(Seq("a", "b", "c")) should be(Seq("a", "b", "c").map{(_, 1)})
      Utils.encode(Seq("a", "b", "a")) should be(Seq("a", "b", "a").map{(_, 1)})
    }

    it ("should encode correctly consecutive duplicates") {
      Utils.encode(Seq("a", "b", "b")) should be(Seq(("a", 1), ("b", 2)))
      Utils.encode(Seq("a", "a", "a")) should be(Seq(("a", 3)))
      Utils.encode(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(("a", 2), ("b", 1), ("c", 3), ("d", 1)))
      Utils.encode(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq((1, 2), (2, 1), (3, 3), (4, 1), (5, 2)))
    }
  }

  describe("encodeModified") {
    it ("should encode Empty sequences into Nil") {
      Utils.encodeModified(Nil) should be(Nil)
    }

    it ("should not encode elements which doesn't have adjacent duplicate") {
      Utils.encodeModified(Seq("a", "b", "c")) should be(Seq("a", "b", "c"))
      Utils.encodeModified(Seq("a", "b", "a")) should be(Seq("a", "b", "a"))
    }

    it ("should encode correctly only consecutive duplicates") {
      Utils.encodeModified(Seq("a", "b", "b")) should be(Seq("a", ("b", 2)))
      Utils.encodeModified(Seq("a", "a", "a")) should be(Seq(("a", 3)))
      Utils.encodeModified(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(("a", 2), "b", ("c", 3), "d"))
      Utils.encodeModified(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq((1, 2), 2, (3, 3), 4, (5, 2)))
    }
  }
}