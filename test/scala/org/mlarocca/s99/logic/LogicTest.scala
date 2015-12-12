package org.mlarocca.s99.logic

import org.scalatest._

class LogicTableTest extends FunSpec with Matchers {

  describe("equality") {
    it("should compare correctly Booleans") {
      LogicTable(false) should equal(false)
      LogicTable(true) should equal(true)

      LogicTable(true) should not equal (false)
      LogicTable(false) should not equal (true)
    }

    it("should compare correctly two LogicTable") {
      LogicTable(false) == LogicTable(false) should be(right = true)
      LogicTable(true) == LogicTable(true) should be(right = true)
      LogicTable(false) == LogicTable(true) should be(right = false)
    }
  }

  describe("not") {
    it("should correctly compute negation") {
      LogicTable(false).not() should equal(true)
      LogicTable(true).not() should equal(false)
    }
  }

  describe("or") {
    it("should correctly compute or") {
      LogicTable(false).or(LogicTable(false)) should equal(false)
      LogicTable(false).or(LogicTable(true)) should equal(true)
      LogicTable(true).or(LogicTable(false)) should equal(true)
      LogicTable(true).or(LogicTable(true)) should equal(true)
    }
  }

  describe("and") {
    it("should correctly compute and") {
      LogicTable(false).and(LogicTable(false)) should equal(false)
      LogicTable(false).and(LogicTable(true)) should equal(false)
      LogicTable(true).and(LogicTable(false)) should equal(false)
      LogicTable(true).and(LogicTable(true)) should equal(true)
    }
  }

  describe("equ") {
    it("should correctly compute equality") {
      LogicTable(false).equ(LogicTable(false)) should equal(true)
      LogicTable(false).equ(LogicTable(true)) should equal(false)
      LogicTable(true).equ(LogicTable(false)) should equal(false)
      LogicTable(true).equ(LogicTable(true)) should equal(true)
    }
  }

  describe("xor") {
    it("should correctly compute xor") {
      LogicTable(false).xor(LogicTable(false)) should equal(false)
      LogicTable(false).xor(LogicTable(true)) should equal(true)
      LogicTable(true).xor(LogicTable(false)) should equal(true)
      LogicTable(true).xor(LogicTable(true)) should equal(false)
    }
  }

  describe("nor") {
    it("should correctly compute nor") {
      LogicTable(false).nor(LogicTable(false)) should equal(true)
      LogicTable(false).nor(LogicTable(true)) should equal(false)
      LogicTable(true).nor(LogicTable(false)) should equal(false)
      LogicTable(true).nor(LogicTable(true)) should equal(false)
    }
  }

  describe("nand") {
    it("should cxorrectly compute nand") {
      LogicTable(false).nand(LogicTable(false)) should equal(true)
      LogicTable(false).nand(LogicTable(true)) should equal(true)
      LogicTable(true).nand(LogicTable(false)) should equal(true)
      LogicTable(true).nand(LogicTable(true)) should equal(false)
    }
  }

  describe("impl") {
    it("should correctly compute implication") {
      LogicTable(false).impl(LogicTable(false)) should equal(true)
      LogicTable(false).impl(LogicTable(true)) should equal(true)
      LogicTable(true).impl(LogicTable(false)) should equal(false)
      LogicTable(true).impl(LogicTable(true)) should equal(true)
    }
  }
}

class CodesTest extends FunSpec with Matchers {

  describe("gray + closure") {
    it ("should throw IllegalArgumentException for non positive Int") {
      a[IllegalArgumentException] should be thrownBy {
        Codes.gray(-1)
      }
      a[IllegalArgumentException] should be thrownBy {
        Codes.gray(0)
      }
    }

    it("should correctly compute gray sequence for any length") {
      Codes.gray(1) should equal( Seq("0", "1"))
      Codes.gray(2) should equal( Seq("00", "01", "11", "10"))
      Codes.gray(3) should equal( Seq("000", "001", "011", "010", "110", "111", "101", "100"))
    }
  }

  describe("huffman") {
    it("should return Nil on empty lists") {
      Codes.huffman(Nil) should be(Nil)
    }

    it("should return a list with one couple for singletons") {
      Codes.huffman(Seq(("abc", 1))) should equal(Seq(("abc", "0")))
    }

    it("should correctly compute huffman code") {
      Codes.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) should
        equal(Seq(("a","0"), ("b","101"), ("c","100"), ("d","111"), ("e","1101"), ("f","1100")))
      Codes.huffman(List(("1", 5), ("2", 7), ("3", 10), ("4", 15), ("5", 20), ("6", 45))) should
        equal(Seq(("1","1010"), ("2","1011"), ("3","100"), ("4","110"), ("5","111"), ("6","0")))
    }
  }
}
