package org.mlarocca.s99.arithmetic

import org.scalatest._
import ArithmeticInt._

import scala.util.Random

class ArithmeticIntTest extends FunSpec with Matchers {
  describe("isPrime") {
    it ("should throw IllegalArgumentException for non positive Int") {
      a[IllegalArgumentException] should be thrownBy {
        (-1).isPrime()
      }
      a[IllegalArgumentException] should be thrownBy {
        (0).isPrime()
      }
    }

    it ("should return the correct value for positive Int") {
      1.isPrime should be(false)
      2.isPrime should be(true)
      3.isPrime should be(true)
      4.isPrime should be(false)
      5.isPrime should be(true)
      6.isPrime should be(false)
      7.isPrime should be(true)
      13.isPrime should be(true)
      25.isPrime should be(false)
      31.isPrime should be(true)
    }
  }
}
