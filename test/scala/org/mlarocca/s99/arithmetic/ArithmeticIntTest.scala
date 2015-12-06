package org.mlarocca.s99.arithmetic

import org.scalatest._
import ArithmeticInt._

import scala.util.Random

class ArithmeticIntTest extends FunSpec with Matchers {
  private val RandomTestIterations = 1000
  private val ProfileIterations = 100

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

      (1 to RandomTestIterations).foreach{ _ =>
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
      (1 to RandomTestIterations).foreach{ _ =>
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

      (1 to RandomTestIterations).foreach { _ =>
        val a = Random.nextInt()
        val b = Random.nextInt()
        a.isCoprime(b) should be(Math.abs(b.gcd(a)) == 1)
      }
    }
  }

  describe("totient") {
    it("should return 0 if value == 0") {
      0.totient() should be(0)
    }

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

    it("should return the correct result if for negative Ints") {
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

  describe("primeFactors") {
    it("should return Nil if value == 0") {
      0.primeFactors() should be(Nil)
    }

    it("should return Nil if value == 1") {
      1.primeFactors() should be(Nil)
    }

    it("should return the correct result for positive values") {
      2.primeFactors() should equal(Seq(2))
      3.primeFactors() should equal(Seq(3))
      4.primeFactors() should equal(Seq(2))
      5.primeFactors() should equal(Seq(5))
      6.primeFactors() should equal(Seq(2, 3))
      7.primeFactors() should equal(Seq(7))
      8.primeFactors() should equal(Seq(2))
      9.primeFactors() should equal(Seq(3))
      10.primeFactors() should equal(Seq(2, 5))
      42.primeFactors() should equal(Seq(2, 3, 7))
    }

    it("should return the correct result if for negative Ints") {
      (-1).primeFactors() should equal(Nil)
      (-2).primeFactors() should equal(Seq(2))
      (-3).primeFactors() should equal(Seq(3))
      (-4).primeFactors() should equal(Seq(2))
      (-5).primeFactors() should equal(Seq(5))
      (-6).primeFactors() should equal(Seq(2, 3))
      (-7).primeFactors() should equal(Seq(7))
      (-8).primeFactors() should equal(Seq(2))
      (-9).primeFactors() should equal(Seq(3))
      (-10).primeFactors() should equal(Seq(2, 5))
      (-42).primeFactors() should equal(Seq(2, 3, 7))
    }
  }

  describe("primeFactorsMultiplicity") {
    it("should return Nil if value == 0") {
      0.primeFactorsMultiplicity() should be(Map.empty)
    }

    it("should return Nil if value == 1") {
      1.primeFactorsMultiplicity() should be(Map.empty)
    }

    it("should return the correct result for positive values") {
      2.primeFactorsMultiplicity() should equal(Map(2 -> 1))
      3.primeFactorsMultiplicity() should equal(Map(3 -> 1))
      4.primeFactorsMultiplicity() should equal(Map(2 -> 2))
      5.primeFactorsMultiplicity() should equal(Map(5 -> 1))
      6.primeFactorsMultiplicity() should equal(Map(2 -> 1, 3-> 1))
      7.primeFactorsMultiplicity() should equal(Map(7 -> 1))
      8.primeFactorsMultiplicity() should equal(Map(2 -> 3))
      9.primeFactorsMultiplicity() should equal(Map(3 -> 2))
      10.primeFactorsMultiplicity() should equal(Map(2 -> 1, 5 -> 1))
      42.primeFactorsMultiplicity() should equal(Map(2 -> 1, 7 -> 1, 3 -> 1))
      126.primeFactorsMultiplicity() should equal(Map(2 -> 1, 7 -> 1, 3 -> 2))
    }

    it("should return the correct result if for negative Ints") {
      (-2).primeFactorsMultiplicity() should equal(Map(2 -> 1))
      (-3).primeFactorsMultiplicity() should equal(Map(3 -> 1))
      (-4).primeFactorsMultiplicity() should equal(Map(2 -> 2))
      (-5).primeFactorsMultiplicity() should equal(Map(5 -> 1))
      (-6).primeFactorsMultiplicity() should equal(Map(2 -> 1, 3-> 1))
    }
  }

  describe("totientFast") {
    it("should return 0 if value == 0") {
      0.totientFast() should be(0)
    }

    it("should return the correct result for positive values") {
      1.totientFast() should be(1)
      2.totientFast() should be(1)
      3.totientFast() should be(2)
      7.totientFast() should be(6)
      8.totientFast() should be(4)
      9.totientFast() should be(6)
      10.totientFast() should be(4)
      42.totientFast() should be(12)
    }

    it("should return the correct result if for negative Ints") {
      -1.totientFast() should be(1)
      -2.totientFast() should be(1)
      -3.totientFast() should be(2)
      -7.totientFast() should be(6)
      -8.totientFast() should be(4)
      -9.totientFast() should be(6)
      -10.totientFast() should be(4)
      -42.totientFast() should be(12)
    }

    it("should match the result of .totient() [randomTest]") {
      (1 to RandomTestIterations).foreach { _ =>
        val a = Random.nextInt(1000)
        a.totient() should be(a.totientFast())
      }
    }

    it("should be sensibly faster than .totient() on large Ints") {
      val testValue = 10090
      val ProfileResult(r1, t1) = profile {testValue.totient()}
      val ProfileResult(r2, t2) = profile {testValue.totientFast()}

      println(s"value: $testValue | Time `.totient()`: $t1 | time ns`.totientFast()`: $t2 ns")

      r1 should equal(r2)
      t2 < t1 should be(true)
    }

    it("profile comparison to totient [randomProfiler]") {
      (1 to ProfileIterations).foreach { _ =>
        val a = Random.nextInt(1000)
        val ProfileResult(_, t1) = profile {a.totient()}
        val ProfileResult(_, t2) = profile {a.totientFast()}
        println(s"value: $a | Time `.totient()`: $t1 | time ns`.totientFast()`: $t2 ns")
      }
    }
  }

  describe("primesTo") {
    it ("should throw IllegalArgumentException if the second index is smaller than the first one") {
      a[IllegalArgumentException] should be thrownBy {
       0.primesTo(-1)
       10.primesTo(8)
        (-1).primesTo(-2)
      }
    }

    it("should return Nil if i == j and i is not prime") {
      0.primesTo(0) should be(Nil)
      1.primesTo(1) should be(Nil)
      10.primesTo(10) should be(Nil)
    }

    it("should return Seq(i) if i == j and i is prime") {
      2.primesTo(2) should be(Seq(2))
      5.primesTo(5) should be(Seq(5))
      13.primesTo(13) should be(Seq(13))
    }

    it("should return the correct set of primes if i < j") {
      0.primesTo(1) should be(Nil)
      0.primesTo(3) should be(Seq(2, 3))
      (-1).primesTo(2) should be(Seq(2))
      5.primesTo(13) should be(Seq(5, 7, 11, 13))
    }
  }

  ////////////////////////////////////////////
  //              Utilities
  ////////////////////////////////////////////

  private case class ProfileResult[R](result: R, time: Long)

  /**
   *
   * Profiler
   *
   * @param block Block of code to be run
   * @tparam R Parameter placeholder for the return type of the code
   * @return A pair with the result of the computation and its runtime, in nanoseconds.
   */
  private def profile[R](block: => R): ProfileResult[R] = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    ProfileResult[R](result, (t1 - t0))
  }
}