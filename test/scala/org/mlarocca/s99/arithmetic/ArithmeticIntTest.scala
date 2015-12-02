package org.mlarocca.s99.arithmetic

import org.scalatest._
import ArithmeticInt._

import scala.util.Random

class ArithmeticIntTest extends FunSpec with Matchers {
  private val IterationNumber = 1000

  describe("equality") {
    it ("should compare correctly Ints, Bytes, Longs etc... ") {
      1 == ArithmeticIntToInt(1) should be(right = true)
      1L == ArithmeticIntToInt(1) should be(right = true)
      1.toByte == ArithmeticIntToInt(1) should be(right = true)
      1.0 == ArithmeticIntToInt(1) should be(right = true)

      1 == ArithmeticIntToInt(2) should be(right = false)
      1L == ArithmeticIntToInt(2) should be(right = false)
      1.toByte == ArithmeticIntToInt(2) should be(right = false)
      1.0 == ArithmeticIntToInt(2) should be(right = false)
    }

    it ("should compare correctly two ArithmeticInts") {
      ArithmeticIntToInt(1) == ArithmeticIntToInt(1) should be(right = true)
      ArithmeticIntToInt(1) == ArithmeticIntToInt(2) should be(right = false)
    }
  }
  describe("isPrime") {
    it ("should throw IllegalArgumentException for non positive Int") {
      a[IllegalArgumentException] should be thrownBy {
        (-1).isPrime()
      }
      a[IllegalArgumentException] should be thrownBy {
        0.isPrime()
      }
    }

    it ("should return the correct value for positive Int") {
      1.isPrime should be(right = false)
      2.isPrime should be(right = true)
      3.isPrime should be(right = true)
      4.isPrime should be(right = false)
      5.isPrime should be(right = true)
      6.isPrime should be(right = false)
      7.isPrime should be(right = true)
      13.isPrime should be(right = true)
      25.isPrime should be(right = false)
      31.isPrime should be(right = true)
    }
  }

  describe("isProbablyPrime") {
    it ("should throw IllegalArgumentException for non positive Int") {
      a[IllegalArgumentException] should be thrownBy {
        (-1).isProbablyPrime()
      }
      a[IllegalArgumentException] should be thrownBy {
        0.isProbablyPrime()
      }
    }

    it ("should return the correct value for positive Int") {
      1.isProbablyPrime() should be(right = false)
      2.isProbablyPrime() should be(right = true)
      3.isProbablyPrime() should be(right = true)
      4.isProbablyPrime() should be(right = false)
      5.isProbablyPrime() should be(right = true)
      6.isProbablyPrime() should be(right = false)
      7.isProbablyPrime() should be(right = true)
      13.isProbablyPrime() should be(right = true)
      25.isProbablyPrime() should be(right = false)
      26.isProbablyPrime() should be(right = false)
      31.isProbablyPrime() should be(right = true)

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

  describe("isCoprime") {
    it("should return the correct result for positive values") {
      1.isCoprime(3) should be(right = true)
      2.isCoprime(3) should be(right = true)
      2.isCoprime(6) should be(right = false)
      3.isCoprime(9) should be(right = false)
      42.isCoprime(63) should be(right = false)
      7.isCoprime(5) should be(right = true)
      13.isCoprime(11) should be(right = true)
    }

    it("should return the correct result if one or both arguments are negative Ints") {
      (-1).isCoprime(3) should be(right = true)
      2.isCoprime(-3) should be(right = true)
      (-2).isCoprime(6) should be(right = false)
      (-3).isCoprime(-7) should be(right = true)
      42.isCoprime(-63) should be(right = false)
    }

    it("should be true iff gcd is 1 [randomTest]") {

      (1 to IterationNumber).foreach { _ =>
        val a = Random.nextInt()
        val b = Random.nextInt()
        a.isCoprime(b) should be(Math.abs(b.gcd(a)) == 1)
      }
    }
  }

  describe("totient") {
    it("should return the correct result for positive values") {
      1.totient() should be(1)
      2.totient() should be(1)
      3.totient() should be(2)
      7.totient() should be(6)
      8.totient() should be(4)
      9.totient() should be(6)
      10.totient() should be(4)
      42.totient() should be(12)
    }

    it("should return the correct result if one or both arguments are negative Ints") {
      -1.totient() should be(1)
      -2.totient() should be(1)
      -3.totient() should be(2)
      -7.totient() should be(6)
      -8.totient() should be(4)
      -9.totient() should be(6)
      -10.totient() should be(4)
      -42.totient() should be(12)
    }
  }
  
}