package org.mlarocca.s99.logic

import org.scalatest._
import LogicTable._

import scala.util.Random

class ArithmeticIntTest extends FunSpec with Matchers {
class LogicTableTest {

}
  describe("equality") {
    it("should compare correctly Booleans") {
      false == LogicTable(false) should be(right = true)
      true == LogicTable(true) should be(right = true)

      false == LogicTable(true) should be(right = false)
    }

    it ("should compare correctly two LogicTable") {
      LogicTable(false) == LogicTable(false) should be(right = true)
      LogicTable(true) == LogicTable(true) should be(right = true)
      LogicTable(false) == LogicTable(true) should be(right = false)
    }
  }
}
