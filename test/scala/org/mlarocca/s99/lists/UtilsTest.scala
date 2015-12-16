package com.mlarocca.s99.lists

import org.scalatest._

import scala.util.Random

class UtilsTest extends FunSpec with Matchers {
  private val Iterations = 10000

  describe("last") {
    it("should return the last value in a non empty sequence") {
      val s = Seq(1, 2, 3)
      val x = Utils.last(s)
      x.shouldBe(3)
      val y = Utils.last(Seq("1"))
      y.shouldBe("1")
    }

    it("should throw NoSuchElementException if an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      a[NoSuchElementException] should be thrownBy {
        Utils.last(emptySeq)
      }
    }
  }

  describe("lastOption") {
    it("should return the last value in a non empty sequence") {
      val s = Seq(1, 2, 3)
      val x = Utils.lastOption(s)
      x.shouldBe(Some(3))
      val y = Utils.lastOption(Seq("1"))
      y.shouldBe(Some("1"))
    }

    it("should return None when an empty sequence is passed") {
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

    it("should throw NoSuchElementException if an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      a[NoSuchElementException] should be thrownBy {
        Utils.penultimate(emptySeq)
      }
    }

    it("should throw NoSuchElementException if a sequence with only 1 element is passed") {
      a[NoSuchElementException] should be thrownBy {
        Utils.penultimate(Seq(1))
      }
    }
  }

  describe("penultimateOption") {
    it("should return the second to last value in a sequence with at least 2 elements") {
      val s = Seq(1, 2, 3)
      val x = Utils.penultimateOption(s)
      x.shouldBe(Some(2))
      val y = Utils.penultimateOption(Seq("1", "2"))
      y.shouldBe(Some("1"))
    }

    it("should return None when an empty sequence is passed") {
      val emptySeq = Seq.empty[String]
      Utils.penultimateOption(emptySeq) should be(None)
    }

    it("should return None when a sequence with a singol element is passed") {
      Utils.penultimateOption(Seq(false)) should be(None)
    }
  }

  describe("nth") {
    it("should return the nth value in a sequence with at least n+1 elements") {
      val s = Seq(1, 2, 3)
      Utils.nth(1, s) shouldBe (2)
      Utils.nth(0, Seq("1", 2)) shouldBe ("1")
      Utils.nth(0, s) shouldBe (1)
      Utils.nth(2, s) shouldBe (3)
    }

    it("should throw NoSuchElementException if an empty sequence is passed") {
      a[NoSuchElementException] should be thrownBy {
        Utils.nth(0, Seq.empty[String])
      }
    }

    it("should throw IllegalArgumentException if a negative index is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.nth(-1, Seq(1, 2, 3))
      }

      a[IllegalArgumentException] should be thrownBy {
        Utils.nth(-1, Nil)
      }
    }

    it("should throw NoSuchElementException if a sequence with too few elements is passed") {
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
    it("should return the nth value in a sequence with at least n+1 elements") {
      val s = Seq(1, 2, 3)
      val x = Utils.nthOption(1, s)
      x.shouldBe(Some(2))
      val y = Utils.nthOption(0, Seq("1", "2"))
      y.shouldBe(Some("1"))
    }

    it("should throw IllegalArgumentException if a negative index is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.nthOption(-1, Seq(1, 2, 3))
      }

      a[IllegalArgumentException] should be thrownBy {
        Utils.nthOption(-1, Nil)
      }
    }

    it("should return None if a sequence with too few elements is passed") {
      Utils.nthOption(1, Seq(1)) should be(None)
      Utils.nthOption(2, Seq(1, 2)) should be(None)
      Utils.nthOption(3, Seq(1, 2)) should be(None)
    }
  }

  describe("length") {
    it("should be 0 for empty Seqs") {
      Utils.length(Nil) should be(0)
    }

    it("should match Seq size") {
      Utils.length(Seq("a")) should be(1)
      Utils.length(Seq("a", 2)) should be(2)
      Utils.length(Seq(false, Utils.length _, 41)) should be(3)
    }
  }

  describe("reverse") {
    it("reversing an empty Seq should return Nil") {
      Utils.reverse(Nil) should be(Nil)
    }

    it("should correctly reverse Seq of any Type") {
      Utils.reverse(Seq("a")) should equal(Seq("a"))
      Utils.reverse(Seq("a", 2)) should equal(Seq(2, "a"))
      Utils.reverse(Seq(false, "true", 41)) should equal(Seq(41, "true", false))
    }
  }

  describe("isPalindrome") {
    it("should return true for Empty sequences") {
      Utils.isPalindrome(Nil) should be(true)
    }

    it("should return true for singletons") {
      Utils.isPalindrome(Seq("a")) should be(true)
      Utils.isPalindrome(Seq(2)) should be(true)
    }

    it("should return true for palindrome sequences") {
      Utils.isPalindrome(Seq("a", "a")) should be(true)
      Utils.isPalindrome(Seq("a", 2, "a")) should be(true)
      Utils.isPalindrome(Seq("a", 2, 2, "a")) should be(true)
      Utils.isPalindrome(Seq("a", 2, false, 2, "a")) should be(true)
    }

    it("should return false for non palindrome sequences") {
      Utils.isPalindrome(Seq("a", "b")) should be(right = false)
      Utils.isPalindrome(Seq("a", 2, 2)) should be(false)
      Utils.isPalindrome(Seq("a", 1, 2, "a")) should be(false)
      Utils.isPalindrome(Seq(true, 2, false, 2, "a")) should be(false)
    }
  }

  describe("flatten") {
    it("should flatten Empty sequences into Nil") {
      Utils.flatten(Nil) should be(Nil)
    }

    it("should flatten simple list returning the sequence itself") {
      Utils.flatten(Seq("a")) should be(Seq("a"))
      Utils.flatten(Seq("a", 2)) should be(Seq("a", 2))
      Utils.flatten(Seq(false, "a", 2)) should be(Seq(false, "a", 2))
    }
    it("should flatten out empty nested lists") {
      Utils.flatten(Seq("a", Nil)) should be(Seq("a"))
      Utils.flatten(Seq(Nil, "a", 2)) should be(Seq("a", 2))
      Utils.flatten(Seq(false, Nil, "a", 2)) should be(Seq(false, "a", 2))
    }

    it("should flatten second order nested lists returning the sequence of elements") {
      Utils.flatten(Seq("a", Seq("b", 2))) should be(Seq("a", "b", 2))
      Utils.flatten(Seq("a", Nil, 2)) should be(Seq("a", 2))
      Utils.flatten(Seq(Seq("b", 2, false), "a", Seq("b", 2))) should be(Seq("b", 2, false, "a", "b", 2))
    }

    it("should flatten higher order nested lists returning the sequence of elements") {
      Utils.flatten(Seq("a", Seq(Seq("b", Nil, 2), 2))) should be(Seq("a", "b", 2, 2))
      Utils.flatten(Seq(Seq("a", Seq("b", Seq(Seq("c", Nil, 1), 2)), false, Seq("d", Seq(Seq("e", Nil, 3), 4))), "f", Seq("g", 5))) should be(Seq("a", "b", "c", 1, 2, false, "d", "e", 3, 4, "f", "g", 5))
    }
  }

  describe("compress") {
    it("should compress Empty sequences into Nil") {
      Utils.compress(Nil) should be(Nil)
    }

    it("should not change lists with no consecutive duplicate") {
      Utils.compress(Seq("a", "b", "c")) should be(Seq("a", "b", "c"))
      Utils.compress(Seq("a", "b", "a")) should be(Seq("a", "b", "a"))
    }

    it("should remove consecutive duplicates") {
      Utils.compress(Seq("a", "b", "b")) should be(Seq("a", "b"))
      Utils.compress(Seq("a", "a", "a")) should be(Seq("a"))
      Utils.compress(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq("a", "b", "c", "d"))
      Utils.compress(Seq("a", "a", "b", "c", "c", "c", "d", "a", "a")) should be(Seq("a", "b", "c", "d", "a"))
    }
  }

  describe("pack") {
    it("should pack Empty sequences into Nil") {
      Utils.pack(Nil) should be(Nil)
    }

    it("should pack lists with no consecutive duplicate") {
      Utils.pack(Seq("a", "b", "c")) should be(Seq("a", "b", "c").map {
        Seq(_)
      })
      Utils.pack(Seq("a", "b", "a")) should be(Seq("a", "b", "a").map {
        Seq(_)
      })
    }

    it("should pack consecutive duplicates") {
      Utils.pack(Seq("a", "b", "b")) should be(Seq(Seq("a"), Seq("b", "b")))
      Utils.pack(Seq("a", "a", "a")) should be(Seq(Seq("a", "a", "a")))
      Utils.pack(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(Seq("a", "a"), Seq("b"), Seq("c", "c", "c"), Seq("d")))
      Utils.pack(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq(Seq(1, 1), Seq(2), Seq(3, 3, 3), Seq(4), Seq(5, 5)))
    }
  }

  describe("encode") {
    it("should encode Empty sequences into Nil") {
      Utils.encode(Nil) should be(Nil)
    }

    it("should encode correctly lists with no consecutive duplicate") {
      Utils.encode(Seq("a", "b", "c")) should be(Seq("a", "b", "c").map {
        (_, 1)
      })
      Utils.encode(Seq("a", "b", "a")) should be(Seq("a", "b", "a").map {
        (_, 1)
      })
    }

    it("should encode correctly consecutive duplicates") {
      Utils.encode(Seq("a", "b", "b")) should be(Seq(("a", 1), ("b", 2)))
      Utils.encode(Seq("a", "a", "a")) should be(Seq(("a", 3)))
      Utils.encode(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(("a", 2), ("b", 1), ("c", 3), ("d", 1)))
      Utils.encode(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq((1, 2), (2, 1), (3, 3), (4, 1), (5, 2)))
    }
  }

  describe("encodeModified") {
    it("should encode Empty sequences into Nil") {
      Utils.encodeModified(Nil) should be(Nil)
    }

    it("should not encode elements which doesn't have adjacent duplicate") {
      Utils.encodeModified(Seq("a", "b", "c")) should be(Seq("a", "b", "c"))
      Utils.encodeModified(Seq("a", "b", "a")) should be(Seq("a", "b", "a"))
    }

    it("should encode correctly only consecutive duplicates") {
      Utils.encodeModified(Seq("a", "b", "b")) should be(Seq("a", ("b", 2)))
      Utils.encodeModified(Seq("a", "a", "a")) should be(Seq(("a", 3)))
      Utils.encodeModified(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(("a", 2), "b", ("c", 3), "d"))
      Utils.encodeModified(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq((1, 2), 2, (3, 3), 4, (5, 2)))
    }
  }


  describe("decode") {
    it("should decode Empty sequences into Nil") {
      Utils.decode(Nil) should be(Nil)
    }

    it("should correctly decode by adding duplicates") {
      Utils.decode(Seq(("a", 1), ("b", 2), ("c", 3))) should be(Seq("a", "b", "b", "c", "c", "c"))
    }


    it("should correctly handle 0 repetitions") {
      Utils.decode(Seq(("a", 1), ("b", 0), ("c", 3), ("a", 2))) should be(Seq("a", "c", "c", "c", "a", "a"))
    }

    it("should throw IllegalArgumentException if a negative number is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.decode(Seq(("a", 1), ("b", -10), ("c", 3)))
      }
    }
  }

  describe("encodeDirect") {
    it("should encode Empty sequences into Nil") {
      Utils.encodeDirect(Nil) should be(Nil)
    }

    it("should encode correctly lists with no consecutive duplicate") {
      Utils.encodeDirect(Seq("a", "b", "c")) should be(Seq("a", "b", "c").map {
        (_, 1)
      })
      Utils.encodeDirect(Seq("a", "b", "a")) should be(Seq("a", "b", "a").map {
        (_, 1)
      })
    }

    it("should encode correctly consecutive duplicates") {
      Utils.encodeDirect(Seq("a", "b", "b")) should be(Seq(("a", 1), ("b", 2)))
      Utils.encodeDirect(Seq("a", "a", "a")) should be(Seq(("a", 3)))
      Utils.encodeDirect(Seq("a", "a", "b", "c", "c", "c", "d")) should be(Seq(("a", 2), ("b", 1), ("c", 3), ("d", 1)))
      Utils.encodeDirect(Seq(1, 1, 2, 3, 3, 3, 4, 5, 5)) should be(Seq((1, 2), (2, 1), (3, 3), (4, 1), (5, 2)))
    }
  }

  describe("duplicate") {
    it("should encode Empty sequences into Nil") {
      Utils.duplicate(Nil) should be(Nil)
    }

    it("should duplicate correctly lists") {
      Utils.duplicate(Seq("a", "b", "c")) should be(Seq("a", "a", "b", "b", "c", "c"))
      Utils.duplicate(Seq("a", "b", "a")) should be(Seq("a", "a", "b", "b", "a", "a"))
    }

    it("should encode correctly lists with duplicates") {
      Utils.duplicate(Seq("a", "b", "b")) should be(Seq("a", "a", "b", "b", "b", "b"))
      Utils.duplicate(Seq("a", "a", "a")) should be(Seq.fill(6)("a"))
      Utils.duplicate(Seq(1, 1, 2, 5)) should be(Seq(1, 1, 1, 1, 2, 2, 5, 5))
    }
  }

  describe("duplicateDirect") {
    it("should encode Empty sequences into Nil") {
      Utils.duplicateDirect(Nil) should be(Nil)
    }

    it("should duplicate correctly lists") {
      Utils.duplicateDirect(Seq("a", "b", "c")) should be(Seq("a", "a", "b", "b", "c", "c"))
      Utils.duplicateDirect(Seq("a", "b", "a")) should be(Seq("a", "a", "b", "b", "a", "a"))
    }

    it("should match duplicate on random lists [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to Random.nextInt(10)).toList map (_ => Random.nextInt())
        Utils.duplicate(s) should equal(Utils.duplicateDirect(s))
      }
    }
  }

  describe("duplicateN") {
    it("should throw IllegalArgumentException if a negative multiplier is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.duplicateN(-1, Seq(1, 2, 3))
        Utils.duplicateN(-10, Nil)
      }
    }

    it("should return Nil for Empty sequences and valid values for `n`") {
      Utils.duplicateN(3, Nil) should be(Nil)
      Utils.duplicateN(0, Nil) should be(Nil)
    }

    it("should return Nil if n == 0, independently on the list") {
      Utils.duplicateN(0, Seq("a")) should be(Nil)
      Utils.duplicateN(0, Nil) should be(Nil)
    }

    it("should duplicate correctly lists when n == 2  [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to Random.nextInt(10)).toList map (_ => Random.nextInt())
        Utils.duplicate(s) should equal(Utils.duplicateN(2, s))
      }
    }

    it("should duplicate correctly lists without duplicates") {
      Utils.duplicateN(3, Seq("a", "b")) should be(Seq("a", "a", "a", "b", "b", "b"))
      Utils.duplicateN(5, Seq("a")) should be(Seq.fill(5)("a"))
      Utils.duplicateN(4, Seq(1, 2, 3)) should be((Seq(1, 2, 3)).flatMap(Seq.fill(4)(_)))
    }

    it("should duplicate correctly lists with duplicates") {
      Utils.duplicateN(3, Seq("a", "b", "b")) should be(Seq("a", "a", "a", "b", "b", "b", "b", "b", "b"))
      Utils.duplicateN(15, Seq("a", "a")) should be(Seq.fill(30)("a"))
    }
  }

  describe("dropN") {
    it("should throw IllegalArgumentException if a non-positive index is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.dropN(-1, Seq(1, 2, 3))
        Utils.dropN(0, Nil)
      }
    }

    it("should return Nil for Empty sequences and valid values for `n`") {
      Utils.dropN(3, Nil) should be(Nil)
      Utils.dropN(1, Nil) should be(Nil)
    }

    it("should drop the right elements") {
      Utils.dropN(1, Seq(1, 2, 3, 4, 5, 6)) should be(Nil)
      Utils.dropN(2, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 3, 5))
      Utils.dropN(3, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 4, 5))
      Utils.dropN(4, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 5, 6))
      Utils.dropN(5, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 6))
      Utils.dropN(6, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 5))
      Utils.dropN(7, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 5, 6))
    }
  }

  describe("split") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.split(-1, Seq(1, 2, 3))
        Utils.split(-10, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if n is bigger than s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.split(4, Seq(1, 2, 3))
        Utils.split(1, Nil)
      }
    }

    it("should return the full list as second argument for n == 0") {
      Utils.split(0, Nil) should be((Nil, Nil))
      Utils.split(0, Seq(1, 2)) should be((Nil, Seq(1, 2)))
    }

    it("should return the full list as first argument for n == s.length") {
      Utils.split(2, Seq(1, 2)) should be((Seq(1, 2), Nil))
    }

    it("should correclty split the list") {
      Utils.split(1, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1), Seq(2, 3, 4, 5, 6)))
      Utils.split(2, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2), Seq(3, 4, 5, 6)))
      Utils.split(3, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 3), Seq(4, 5, 6)))
      Utils.split(4, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 3, 4), Seq(5, 6)))
      Utils.split(5, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 3, 4, 5), Seq(6)))
    }
  }

  describe("slice") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.slice(-1, 0, Seq(1, 2, 3))
        Utils.slice(-10, -5, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if j < i") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.slice(1, 0, Seq(1, 2, 3))
        Utils.slice(0, -5, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if i >= s.length or j > s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.slice(0, 4, Seq(1, 2, 3))
        Utils.slice(3, 3, Seq(1, 2, 3))
        Utils.slice(1, 1, Nil)
      }
    }

    it("should return return Nil if i == j (== 0)") {
      Utils.slice(0, 0, Seq(1, 2)) should be(Nil)
      Utils.slice(1, 1, Seq(1, 2)) should be(Nil)
      Utils.slice(2, 2, Seq(1, 2, 3)) should be(Nil)
    }

    it("should return the full list if i == 0 and j == s.length") {
      Utils.slice(0, 2, Seq(1, 2)) should be(Seq(1, 2))
      Utils.slice(0, 4, Seq("a", "b", "c", "d")) should be(Seq("a", "b", "c", "d"))
    }

    it("should correclty slice the list") {
      Utils.slice(0, 2, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(0, 1))
      Utils.slice(0, 3, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(0, 1, 2))
      Utils.slice(1, 3, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(1, 2))
      Utils.slice(3, 5, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(3, 4))
      Utils.slice(3, 6, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(3, 4, 5))
      Utils.slice(3, 7, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(3, 4, 5, 6))
    }
  }

  describe("sliceDirect") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.sliceDirect(-1, 0, Seq(1, 2, 3))
        Utils.sliceDirect(-10, -5, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if j < i") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.sliceDirect(1, 0, Seq(1, 2, 3))
        Utils.sliceDirect(0, -5, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if i >= s.length or j > s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.sliceDirect(0, 4, Seq(1, 2, 3))
        Utils.sliceDirect(3, 3, Seq(1, 2, 3))
        Utils.sliceDirect(1, 1, Nil)
      }
    }

    it("should return return Nil if i == j (== 0)") {
      Utils.sliceDirect(0, 0, Seq(1, 2)) should be(Nil)
      Utils.sliceDirect(1, 1, Seq(1, 2)) should be(Nil)
      Utils.sliceDirect(2, 2, Seq(1, 2, 3)) should be(Nil)
    }

    it("should return the full list if i == 0 and j == s.length") {
      Utils.sliceDirect(0, 2, Seq(1, 2)) should be(Seq(1, 2))
      Utils.sliceDirect(0, 4, Seq("a", "b", "c", "d")) should be(Seq("a", "b", "c", "d"))
    }

    it("should correclty slice the list") {
      Utils.sliceDirect(0, 2, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(0, 1))
      Utils.sliceDirect(0, 3, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(0, 1, 2))
      Utils.sliceDirect(1, 3, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(1, 2))
      Utils.sliceDirect(3, 5, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(3, 4))
      Utils.sliceDirect(3, 6, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(3, 4, 5))
      Utils.sliceDirect(3, 7, Seq(0, 1, 2, 3, 4, 5, 6)) should be(Seq(3, 4, 5, 6))
    }


    it("should match slice [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to Random.nextInt(10)).toList map (_ => Random.nextInt())
        val i = Random.nextInt(s.size)
        val j = i + Random.nextInt(s.size - i + 1)

        Utils.slice(i, j, s) should equal(Utils.sliceDirect(i, j, s))
      }
    }
  }

  describe("rotate") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.rotate(-1, Seq(1, 2, 3))
        Utils.rotate(-10, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if n is bigger than s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.rotate(4, Seq(1, 2, 3))
        Utils.rotate(1, Nil)
      }
    }

    it("should return the original list for n == 0") {
      Utils.rotate(0, Nil) should be(Nil)
      Utils.rotate(0, Seq(1, 2)) should be(Seq(1, 2))
    }

    it("should return the original list for n == s.length") {
      Utils.rotate(2, Seq(1, 2)) should be(Seq(1, 2))
    }

    it("should correclty rotate the list") {
      Utils.rotate(1, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(2, 3, 4, 5, 6, 1))
      Utils.rotate(2, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(3, 4, 5, 6, 1, 2))
      Utils.rotate(3, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(4, 5, 6, 1, 2, 3))
      Utils.rotate(4, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(5, 6, 1, 2, 3, 4))
      Utils.rotate(5, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(6, 1, 2, 3, 4, 5))
    }
  }

  describe("removeAt") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.removeAt(-1, Seq(1, 2, 3))
        Utils.removeAt(-10, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if i is bigger than s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.removeAt(4, Seq(1, 2, 3))
        Utils.removeAt(1, Nil)
      }
    }

    it("should remove the correct from the list") {
      Utils.removeAt(0, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(2, 3, 4, 5, 6), 1))
      Utils.removeAt(1, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 3, 4, 5, 6), 2))
      Utils.removeAt(2, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 4, 5, 6), 3))
      Utils.removeAt(3, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 3, 5, 6), 4))
      Utils.removeAt(4, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 3, 4, 6), 5))
      Utils.removeAt(5, Seq(1, 2, 3, 4, 5, 6)) should be((Seq(1, 2, 3, 4, 5), 6))
    }
  }

  describe("insertAt") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.insertAt("el", -1, Seq(1, 2, 3))
        Utils.insertAt("el", -10, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if i is bigger than s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.insertAt(0, 4, Seq(1, 2, 3))
        Utils.insertAt(1, 1, Nil)
      }
    }

    it("should insert an element as head of an empty list") {
      Utils.insertAt("el", 0, Nil) should be(Seq("el"))
    }

    it("should insert the new element in the right position inside the list") {
      Utils.insertAt("a", 0, Seq(1, 2, 3, 4, 5, 6)) should be(Seq("a", 1, 2, 3, 4, 5, 6))
      Utils.insertAt(7, 1, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 7, 2, 3, 4, 5, 6))
      Utils.insertAt(false, 2, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, false, 3, 4, 5, 6))
      Utils.insertAt(0, 3, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 0, 4, 5, 6))
      Utils.insertAt(0, 4, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 0, 5, 6))
      Utils.insertAt(0, 5, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 5, 0, 6))
      Utils.insertAt(0, 6, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 5, 6, 0))
    }
  }

  describe("insertAtDirect") {
    it("should throw IndexOutOfBoundsException if a negative index is passed") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.insertAtDirect("el", -1, Seq(1, 2, 3))
        Utils.insertAtDirect("el", -10, Nil)
      }
    }

    it("should throw IndexOutOfBoundsException if i is bigger than s.length") {
      a[IndexOutOfBoundsException] should be thrownBy {
        Utils.insertAtDirect(0, 4, Seq(1, 2, 3))
        Utils.insertAtDirect(1, 1, Nil)
      }
    }

    it("should insert an element as head of an empty list") {
      Utils.insertAtDirect("el", 0, Nil) should be(Seq("el"))
    }

    it("should insert the new element in the right position inside the list") {
      Utils.insertAtDirect("a", 0, Seq(1, 2, 3, 4, 5, 6)) should be(Seq("a", 1, 2, 3, 4, 5, 6))
      Utils.insertAtDirect(7, 1, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 7, 2, 3, 4, 5, 6))
      Utils.insertAtDirect(false, 2, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, false, 3, 4, 5, 6))
      Utils.insertAtDirect(0, 3, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 0, 4, 5, 6))
      Utils.insertAtDirect(0, 4, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 0, 5, 6))
      Utils.insertAtDirect(0, 5, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 5, 0, 6))
      Utils.insertAtDirect(0, 6, Seq(1, 2, 3, 4, 5, 6)) should be(Seq(1, 2, 3, 4, 5, 6, 0))
    }

    it("should match insertAt [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to Random.nextInt(10)).toList map (_ => Random.nextString(Random.nextInt(10)))
        val i = Random.nextInt(s.size + 1)
        val el = Random.nextString(Random.nextInt(10))

        Utils.insertAt(el, i, s) should equal(Utils.insertAtDirect(el, i, s))
      }
    }
  }

  describe("range") {
    it ("should throw IllegalArgumentException if the second index is smaller than the first one") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.range(0, -1)
        Utils.range(10, 8)
        Utils.range(-1, -2)
      }
    }

    it("should return Nil if i == j") {
      Utils.range(0, 0) should be(Nil)
      Utils.range(10, 10) should be(Nil)
      Utils.range(-1, -1) should be(Nil)
    }

    it("should return the correct range if i < j") {
      Utils.range(0, 1) should be(Seq(0))
      Utils.range(0, 3) should be(Seq(0, 1, 2))
      Utils.range(-1, 2) should be(Seq(-1, 0, 1))
      Utils.range(-3, -1) should be(Seq(-3, -2))
      Utils.range(-3, 0) should be(Seq(-3, -2, -1))

    }
  }

  describe("randomSelect") {
    it ("should throw IllegalArgumentException if a negative size is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.randomSelect(-1, Nil)
        Utils.randomSelect(-1, Seq(8))
      }
    }

    it ("should throw IllegalArgumentException if n is greather than the size of the list") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.randomSelect(1, Nil)
        Utils.randomSelect(10, Seq(1))
        Utils.randomSelect(2, Seq(1))
      }
    }

    it("should return Nil if n == 0") {
      Utils.randomSelect(0, Seq(1, 2, 3, 4)) should be(Nil)
      Utils.randomSelect(0, Nil) should be(Nil)
    }

    it("should return the correct number of elements, all of them in the list [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to (10 + Random.nextInt(90))).toList map (_ => Random.nextInt(1000))
        val n = Random.nextInt(s.size + 1)
        val xs = Utils.randomSelect(n, s)

        xs.size should be (n)

        val randomElementsCounter = xs.groupBy(x => x).toSeq.map{case (key, values) => (key, values.size)}
        val originalElementsCounter = s.groupBy(x => x).map{case (key, values) => (key, values.size)}

        randomElementsCounter.foreach {
          case (key, count) => (count <= originalElementsCounter(key)) should be(true)
        }
      }
    }

    it("should extract the elements randomly (the std deviation for the counters for each element should be low enough)") {
      val s = Utils.range(0, 100)

      val results = (0 to Iterations).toList.map { i =>
        val n = Random.nextInt(s.size + 1)
        val xs = Utils.randomSelect(n, s)

        xs.size should be (n)

        s.map{ x => (x, xs.count(_ == x).toDouble) }
      }.flatten

      val counters = results.groupBy{case (key, _) => key}.map { case (_, values) => values.map(_._2).sum }
      val (mean, stdDev) = calcAvgAndStddev(counters)

      //Should be less than 1% for 10000 Iterations
      (stdDev <= (mean * 0.01)) should be(true)
    }
  }

  describe("lotto") {
    it ("should throw IllegalArgumentException if a negative size is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.lotto(-1, 0)
        Utils.lotto(-1, 1)
      }
    }

    it ("should throw IllegalArgumentException if extracted is greather than maxValue") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.lotto(1, 0)
        Utils.lotto(10, 4)
        Utils.lotto(2, 1)
      }
    }

    it("should return Nil if extracted == 0") {
      Utils.lotto(0, 0) should be(Nil)
      Utils.lotto(0, 44) should be(Nil)
    }

    it("should return `maxValue` elements if extracted == maxValue") {
      Utils.lotto(1, 1).length should be(1)
      Utils.lotto(44, 44).length should be(44)
    }

    it("should return the correct number of elements, all of them within the range, and at most once [random test]") {
      (1 to 100) foreach { _ =>
        val m = 10 + Random.nextInt(90)
        val n = Random.nextInt(m + 1)
        val xs = Utils.lotto(n, m)

        xs.size should be (n)

        xs.groupBy(x => x).toSeq.map{
          case (key, values) =>
            values.size should be(1)
            key >= 1 && key <= m should be(true)
        }
      }
    }

    it("should extract the elements randomly (the std deviation for the counters for each element should be low enough)") {
      val m = 100

      val results = (0 to Iterations).toList.map { i =>
        val n = Random.nextInt(m + 1)
        val xs = Utils.lotto(n, m)

        xs.size should be (n)

        xs
      }.flatten

      val counters = Utils.range(1, m + 1).map { x =>
        results.count(y => y == x).toDouble
      }
      val (mean, stdDev) = calcAvgAndStddev(counters)

      //Should be less than 1% for 10000 Iterations
      (stdDev <= (mean * 0.01)) should be(true)
    }
  }

  describe("randomPermute") {

    it("should return Nil for empty sequences") {
      Utils.randomPermute(Nil) should be(Nil)
    }

    it("should return the correct number of elements, all of them in the list [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to (10 + Random.nextInt(90))).toList map (_ => Random.nextInt(1000))
        val xs = Utils.randomPermute(s)

        xs.size should be (s.size)
        xs.sorted should equal(s.sorted)
      }
    }

    it("should order the elements randomly (the std deviation for the sum of the positions in the permutation for each element should be low enough)") {
      val n = 100
      val s = Utils.range(0, n)

      val results = (0 to Iterations).toList.map { i =>
        val xs = Utils.randomPermute(s)

        xs.size should be (n)

        s.map{ x => (x, xs.indexOf(x).toDouble) }
      }.flatten

      val counters = results.groupBy{case (key, _) => key}.map { case (_, values) => values.map(_._2).sum }
      val (mean, stdDev) = calcAvgAndStddev(counters)

      //Should be less than 1% for 10000 Iterations
      (stdDev <= (mean * 0.01)) should be(true)
    }
  }

  describe("combinations") {
    it ("should throw IllegalArgumentException if a negative counter is passed") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.combinations(-1, Nil)
        Utils.combinations(-1, Seq(1))
      }
    }

    it ("should return Nil if k is greather than s.size") {
      Utils.combinations(1, Nil) should be(Nil)
      Utils.combinations(10, Seq(1, 2, 0)) should be(Nil)
      Utils.combinations(2, Seq(1)) should be(Nil)
    }

    it("should return Nil if k == 0") {
      Utils.combinations(0, Nil) should be(Nil)
      Utils.combinations(0, Seq(44)) should be(Nil)
    }

    it("should return the full list if k == s.size") {
      Utils.combinations(1, Seq(44)) should be(Seq(Seq(44)))
      Utils.combinations(2, Seq(22, 44)) should be(Seq(Seq(22, 44)))
    }

    it("should return the sequence of singletons if k == 1") {
      Utils.combinations(1, Seq(44)) should be(Seq(Seq(44)))
      Utils.combinations(1, Seq(1, 2, 3)) should be(Seq(Seq(1), Seq(2), Seq(3)))
    }

    it("should return all the combinations for if k == 2") {
      Utils.combinations(2, Seq(1, 2, 3)) should be(Seq(Seq(1, 2), Seq(1, 3), Seq(2, 3)))
    }

    it("should return all the combinations for if k == 3") {
      Utils.combinations(3, Seq(1, 2, 3, 4)) should be(Seq(Seq(1, 2, 3), Seq(1, 2, 4), Seq(1, 3, 4), Seq(2, 3, 4)))
    }
  }

  describe("group") {
    it ("should throw IllegalArgumentException if any of the sizes is non positive") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.group(Seq(-1, 3), Nil)
        Utils.group(Seq(1, 0), Seq(1))
      }
    }

    it ("should throw IllegalArgumentException if the sum of the sizes is not equal to the size of the list") {
      a[IllegalArgumentException] should be thrownBy {
        Utils.group(Seq(1, 3), Nil)
        Utils.group(Seq(1, 2, 3), Seq(2, 3, 6, 1))
      }
    }

    it ("should return the full list is sizes == Seq(s.size)") {
      Utils.group(Seq(1), Seq(1)) should be(Seq(Seq(Seq(1))))
      Utils.group(Seq(2), Seq(1, 0)) should be(Seq(Seq(Seq(1, 0))))
    }

    it("should return the correct breakdown of s") {

      Utils.group(Seq(2, 1), Seq(1, 2, 3)) should equal(Seq(Seq(Seq(1, 2), Seq(3)), Seq(Seq(1, 3), Seq(2)), Seq(Seq(2, 3), Seq(1))))
      Utils.group(Seq(1, 1, 1), Seq(1, 2, 3)) should equal(Seq(Seq(Seq(1), Seq(2), Seq(3)), Seq(Seq(1), Seq(3), Seq(2)), Seq(Seq(2), Seq(1), Seq(3)), Seq(Seq(2), Seq(3), Seq(1)), Seq(Seq(3), Seq(1), Seq(2)), Seq(Seq(3), Seq(2), Seq(1))))

      val s1 = Seq(1, 2, 3, 4, 5)

      val r1 = Seq(Seq(Seq(1, 2), Seq(3, 4, 5)), Seq(Seq(1, 3), Seq(2, 4, 5)), Seq(Seq(1, 4), Seq(2, 3, 5)), Seq(Seq(1, 5), Seq(2, 3, 4)), Seq(Seq(2, 3), Seq(1, 4, 5)), Seq(Seq(2, 4), Seq(1, 3, 5)), Seq(Seq(2, 5), Seq(1, 3, 4)), Seq(Seq(3, 4), Seq(1, 2, 5)), Seq(Seq(3, 5), Seq(1, 2, 4)), Seq(Seq(4, 5), Seq(1, 2, 3)))
      val r2 = Seq(Seq(Seq(1, 2), Seq(3), Seq(4, 5)), Seq(Seq(1, 2), Seq(4), Seq(3, 5)), Seq(Seq(1, 2), Seq(5), Seq(3, 4)), Seq(Seq(1, 3), Seq(2), Seq(4, 5)), Seq(Seq(1, 3), Seq(4), Seq(2, 5)), Seq(Seq(1, 3), Seq(5), Seq(2, 4)), Seq(Seq(1, 4), Seq(2), Seq(3, 5)), Seq(Seq(1, 4), Seq(3), Seq(2, 5)), Seq(Seq(1, 4), Seq(5), Seq(2, 3)), Seq(Seq(1, 5), Seq(2), Seq(3, 4)), Seq(Seq(1, 5), Seq(3), Seq(2, 4)), Seq(Seq(1, 5), Seq(4), Seq(2, 3)), Seq(Seq(2, 3), Seq(1), Seq(4, 5)), Seq(Seq(2, 3), Seq(4), Seq(1, 5)), Seq(Seq(2, 3), Seq(5), Seq(1, 4)), Seq(Seq(2, 4), Seq(1), Seq(3, 5)), Seq(Seq(2, 4), Seq(3), Seq(1, 5)), Seq(Seq(2, 4), Seq(5), Seq(1, 3)), Seq(Seq(2, 5), Seq(1), Seq(3, 4)), Seq(Seq(2, 5), Seq(3), Seq(1, 4)), Seq(Seq(2, 5), Seq(4), Seq(1, 3)), Seq(Seq(3, 4), Seq(1), Seq(2, 5)), Seq(Seq(3, 4), Seq(2), Seq(1, 5)), Seq(Seq(3, 4), Seq(5), Seq(1, 2)), Seq(Seq(3, 5), Seq(1), Seq(2, 4)), Seq(Seq(3, 5), Seq(2), Seq(1, 4)), Seq(Seq(3, 5), Seq(4), Seq(1, 2)), Seq(Seq(4, 5), Seq(1), Seq(2, 3)), Seq(Seq(4, 5), Seq(2), Seq(1, 3)), Seq(Seq(4, 5), Seq(3), Seq(1, 2)))
      val r3 = Seq(Seq(Seq(1), Seq(2, 3), Seq(4), Seq(5)), Seq(Seq(1), Seq(2, 3), Seq(5), Seq(4)), Seq(Seq(1), Seq(2, 4), Seq(3), Seq(5)), Seq(Seq(1), Seq(2, 4), Seq(5), Seq(3)), Seq(Seq(1), Seq(2, 5), Seq(3), Seq(4)), Seq(Seq(1), Seq(2, 5), Seq(4), Seq(3)), Seq(Seq(1), Seq(3, 4), Seq(2), Seq(5)), Seq(Seq(1), Seq(3, 4), Seq(5), Seq(2)), Seq(Seq(1), Seq(3, 5), Seq(2), Seq(4)), Seq(Seq(1), Seq(3, 5), Seq(4), Seq(2)), Seq(Seq(1), Seq(4, 5), Seq(2), Seq(3)), Seq(Seq(1), Seq(4, 5), Seq(3), Seq(2)), Seq(Seq(2), Seq(1, 3), Seq(4), Seq(5)), Seq(Seq(2), Seq(1, 3), Seq(5), Seq(4)), Seq(Seq(2), Seq(1, 4), Seq(3), Seq(5)), Seq(Seq(2), Seq(1, 4), Seq(5), Seq(3)), Seq(Seq(2), Seq(1, 5), Seq(3), Seq(4)), Seq(Seq(2), Seq(1, 5), Seq(4), Seq(3)), Seq(Seq(2), Seq(3, 4), Seq(1), Seq(5)), Seq(Seq(2), Seq(3, 4), Seq(5), Seq(1)), Seq(Seq(2), Seq(3, 5), Seq(1), Seq(4)), Seq(Seq(2), Seq(3, 5), Seq(4), Seq(1)), Seq(Seq(2), Seq(4, 5), Seq(1), Seq(3)), Seq(Seq(2), Seq(4, 5), Seq(3), Seq(1)), Seq(Seq(3), Seq(1, 2), Seq(4), Seq(5)), Seq(Seq(3), Seq(1, 2), Seq(5), Seq(4)), Seq(Seq(3), Seq(1, 4), Seq(2), Seq(5)), Seq(Seq(3), Seq(1, 4), Seq(5), Seq(2)), Seq(Seq(3), Seq(1, 5), Seq(2), Seq(4)), Seq(Seq(3), Seq(1, 5), Seq(4), Seq(2)), Seq(Seq(3), Seq(2, 4), Seq(1), Seq(5)), Seq(Seq(3), Seq(2, 4), Seq(5), Seq(1)), Seq(Seq(3), Seq(2, 5), Seq(1), Seq(4)), Seq(Seq(3), Seq(2, 5), Seq(4), Seq(1)), Seq(Seq(3), Seq(4, 5), Seq(1), Seq(2)), Seq(Seq(3), Seq(4, 5), Seq(2), Seq(1)), Seq(Seq(4), Seq(1, 2), Seq(3), Seq(5)), Seq(Seq(4), Seq(1, 2), Seq(5), Seq(3)), Seq(Seq(4), Seq(1, 3), Seq(2), Seq(5)), Seq(Seq(4), Seq(1, 3), Seq(5), Seq(2)), Seq(Seq(4), Seq(1, 5), Seq(2), Seq(3)), Seq(Seq(4), Seq(1, 5), Seq(3), Seq(2)), Seq(Seq(4), Seq(2, 3), Seq(1), Seq(5)), Seq(Seq(4), Seq(2, 3), Seq(5), Seq(1)), Seq(Seq(4), Seq(2, 5), Seq(1), Seq(3)), Seq(Seq(4), Seq(2, 5), Seq(3), Seq(1)), Seq(Seq(4), Seq(3, 5), Seq(1), Seq(2)), Seq(Seq(4), Seq(3, 5), Seq(2), Seq(1)), Seq(Seq(5), Seq(1, 2), Seq(3), Seq(4)), Seq(Seq(5), Seq(1, 2), Seq(4), Seq(3)), Seq(Seq(5), Seq(1, 3), Seq(2), Seq(4)), Seq(Seq(5), Seq(1, 3), Seq(4), Seq(2)), Seq(Seq(5), Seq(1, 4), Seq(2), Seq(3)), Seq(Seq(5), Seq(1, 4), Seq(3), Seq(2)), Seq(Seq(5), Seq(2, 3), Seq(1), Seq(4)), Seq(Seq(5), Seq(2, 3), Seq(4), Seq(1)), Seq(Seq(5), Seq(2, 4), Seq(1), Seq(3)), Seq(Seq(5), Seq(2, 4), Seq(3), Seq(1)), Seq(Seq(5), Seq(3, 4), Seq(1), Seq(2)), Seq(Seq(5), Seq(3, 4), Seq(2), Seq(1)))

      Utils.group(Seq(2, 3), s1) should equal(r1)
      Utils.group(Seq(2, 1, 2), s1) should equal(r2)
      Utils.group(Seq(1, 2, 1, 1), s1)  should equal(r3)
    }
  }

  describe("lsort") {

    it("should sort sublists of any type and length") {
      Utils.lsort(Nil) should be(Nil)
      Utils.lsort(Seq(Seq(1, "a"), Seq(false))) should equal(Seq(Seq(false), Seq(1, "a")))
      Utils.lsort(Seq(Seq(1), Seq(2.0, 1))) should equal(Seq(Seq(1), Seq(2.0, 1)))
    }

    it("should sort sublists by their length [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to (1 + Random.nextInt(10))).toList.map { _ =>
          (0 to (10 + Random.nextInt(90))).toList map (_ => Random.nextInt(1000))
        }

        val sublistsLengths = Utils.lsort(s).map(_.length)
        sublistsLengths should equal(sublistsLengths.sorted)
      }
    }

    it("should correctly handle empty sublists") {
      Utils.lsort(Seq(Nil, Seq(1, 2), Nil, Seq(1))) should equal(Seq(Nil, Nil,  Seq(1), Seq(1, 2)))
    }
  }

  describe("lsortFreq") {

    it("should sort sublists of any type and length") {
      Utils.lsortFreq(Nil) should be(Nil)
      Utils.lsortFreq(Seq(Seq(1.0), Seq(1, "a"), Seq(false))) should equal(Seq(Seq(1, "a"), Seq(1.0), Seq(false)))
      Utils.lsortFreq(Seq(Seq(1), Seq(2.0, 1))) should equal(Seq(Seq(1), Seq(2.0, 1)))
    }

    it("should sort sublists by their length [random test]") {
      (1 to 100) foreach { _ =>
        val s = (0 to (1 + Random.nextInt(10))).toList.map { _ =>
          (0 to (10 + Random.nextInt(90))).toList map (_ => Random.nextInt(1000))
        }

        var sortedS = Utils.lsortFreq(s)
        val sublistsLengths = sortedS.map(_.length)
        val sublistsFrequencies = sortedS.map(xs => sublistsLengths.count(_ == xs.length))
        sublistsFrequencies should equal(sublistsFrequencies.sorted)
      }
    }

    it("should correctly handle empty sublists") {
      Utils.lsortFreq(Seq(Nil, Seq(1, 2), Nil, Seq(1))) should equal(Seq(Seq(1, 2), Seq(1), Nil, Nil))
    }
  }

  /**
   * UTILITIES
   *
   * @param ts
   * @param num
   * @tparam T
   * @return
   */
  def calcAvgAndStddev[T](ts: Iterable[T])(implicit num: Fractional[T]): (T, Double) = {
    def avg(ts: Iterable[T])(implicit num: Fractional[T]): T = {
      num.div(ts.sum, num.fromInt(ts.size)) // Leaving with type of function T
    }

    val mean = avg(ts) // Leave val type of T
    val stdDev = // Root of mean diffs
      Math.sqrt(num.toDouble(
        ts.foldLeft(num.zero)((b, a) =>
          num.plus(b, num.times(num.minus(a, mean), num.minus(a, mean))))) /
          ts.size)
    (mean, stdDev)
  }
}