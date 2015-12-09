package org.mlarocca.s99.logic

import org.scalatest._

class ArithmeticIntTest extends FunSpec with Matchers {
class LogicTableTest {

}
  describe("equality") {
    it("should compare correctly Booleans") {
      LogicTable(false) should equal(false)
      LogicTable(true) should equal(true)

      LogicTable(true) should not equal(false)
      LogicTable(false) should not equal(true)
    }

    it ("should compare correctly two LogicTable") {
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
