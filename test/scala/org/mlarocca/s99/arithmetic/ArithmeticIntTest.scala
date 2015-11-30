package org.mlarocca.s99.arithmetic

import org.scalatest._
import ArithmeticInt._

import scala.util.Random

class ArithmeticIntTest extends FunSpec with Matchers {
  private val IterationNumber = 1000

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

  describe("isProbablyPrime") {
    it ("should throw IllegalArgumentException for non positive Int") {
      a[IllegalArgumentException] should be thrownBy {
        (-1).isProbablyPrime()
      }
      a[IllegalArgumentException] should be thrownBy {
        (0).isProbablyPrime()
      }
    }

    it ("should return the correct value for positive Int") {
      1.isProbablyPrime() should be(false)
      2.isProbablyPrime() should be(true)
      3.isProbablyPrime() should be(true)
      4.isProbablyPrime() should be(false)
      5.isProbablyPrime() should be(true)
      6.isProbablyPrime() should be(false)
      7.isProbablyPrime() should be(true)
      13.isProbablyPrime() should be(true)
      25.isProbablyPrime() should be(false)
      26.isProbablyPrime() should be(false)
      31.isProbablyPrime() should be(true)

    }


    it ("should return the same result as isPrime [random]") {

      (1 to IterationNumber).foreach{ _ =>
        val n = 1 + Random.nextInt(Int.MaxValue)
        n.isProbablyPrime() should be(n.isPrime())
      }
    }
  }  
}
