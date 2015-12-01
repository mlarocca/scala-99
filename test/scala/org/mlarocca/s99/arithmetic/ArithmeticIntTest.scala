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


    it ("should return the same result as isPrime [randomTest]") {

      (1 to IterationNumber).foreach{ _ =>
        val n = 1 + Random.nextInt(Int.MaxValue)
        n.isProbablyPrime() should be(n.isPrime())
      }
    }
  }

  describe("gcd") {
    it ("should return a when b == 0") {
      1.gcd(0).value should be(1)
      2.gcd(0).value should be(2)
      3.gcd(0).value should be(3)
      0.gcd(0).value should be(0)
    }

    it ("should return b when a == 0") {
      0.gcd(1).value should be(1)
      0.gcd(3).value should be(3)
    }

    it ("should return the correct result for positive values") {
      1.gcd(3).value should be(1)
      2.gcd(3).value should be(1)
      2.gcd(6).value should be(2)
      3.gcd(9).value should be(3)
      42.gcd(63).value should be(21)
      63.gcd(42).value should be(21)
      36.gcd(63).value should be(9)
    }

    it ("should return the correct result if one or both arguments are negative Ints") {
      -1.gcd(3).value should be(-1)
      2.gcd(-3).value should be(-1)
      -2.gcd(6).value should be(-2)
      -3.gcd(-9).value should be(-3)
      42.gcd(-63).value should be(-21)
      -63.gcd(42).value should be(-21)
      -36.gcd(-63).value should be(-9)
    }

    it ("should be simmetric [randomTest]") {

      (1 to IterationNumber).foreach{ _ =>
        val a = Random.nextInt()
        val b = Random.nextInt()
        a.gcd(b) should be(b.gcd(a))
      }
    }
  }
}