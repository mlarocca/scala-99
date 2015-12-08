package org.mlarocca.s99.arithmetic

import javax.swing.plaf.BorderUIResource.EmptyBorderUIResource

import scala.annotation.tailrec
import scala.util.{Try, Random}


object ArithmeticInt {
  implicit def IntToArithmeticInt(i: Int): ArithmeticInt = ArithmeticInt(i)
  implicit def ArithmeticIntToInt(ai: ArithmeticInt): Int = ai.value

  /**
   *
   * @param i
   * @param j
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](NonDecreasingPositiveIndicesIllegalArgumentErrorMessage)
  def goldbachList(i: Int, j: Int): Seq[(ArithmeticInt, ArithmeticInt)] = {
    if (j < i || i < 0) {
      throw new IllegalArgumentException(NonDecreasingPositiveIndicesIllegalArgumentErrorMessage)
    } else {
      //i + i % 2 == smallest even number greather than or equal to `i`
      (Math.max(4, (i + i % 2)).to(j, 2)).toSeq.map(_.goldbachDecomposition())
    }
  }

  private[s99] val NonDecreasingPositiveIndicesIllegalArgumentErrorMessage = "Indices must be positive and in non-decreasing order"
  private[s99] val NegativeValueErrorMessage = "Value must be positive"
  private[s99] val NonDecreasingIndicesIllegalArgumentErrorMessage = "Indices must be in non-decreasing order"
  private[s99] val GoldbachIllegalArgumentException = "Only even numbers greater than 2 can be decomposed following Goldbach 's conjecture"
}

case class ArithmeticInt(value: Int) {
  import ArithmeticInt._

  override def hashCode = value

  override def canEqual(other: Any): Boolean = {
    other.isInstanceOf[ArithmeticInt]
  }

  override def equals(other: Any) = other match {
    case that: ArithmeticInt => (that canEqual this) && this.hashCode == that.hashCode
    case thatValue: Int => this.value == thatValue
    case _ => false
  }

  @throws[IllegalArgumentException]
  def isPrime(): Boolean = value match {
    case 1 =>
      false
    case _ if value <= 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
    case _ =>
      (2 to Math.sqrt(value).toInt).forall {
        value % _ != 0
      }
  }

  /**
   * Miller–Rabin primality test.
   *
   * @param k How many iterations should be run. Precision ~= 2**(-k).
   * @throws IllegalArgumentException(NegativeValueErrorMessage)
   * @return
   */
  @throws[IllegalArgumentException]
  def isProbablyPrime(k: Int = 100): Boolean = {
    val m = value - 1

    /**
     * Scala translation for:
     *  repeat r − 1 times:
     *    x ← x ** 2 mod n
     *    if x = 1 then return composite
     *    if x = n − 1 then break
     *  composite
     *
     * @param r Invariant: r >= 2
     * @param x
     * @return
     */
    @tailrec
    def matchR(r: Int, x: Int): Boolean = (r, BigInt(x).modPow(2, value).toInt) match {
      case (_, 1)|(1, _)|(0, _) => false
      case (_, y) if y == m => true
      case (_, y) => matchR(r - 1, y)
    }

    value match {
      case 1 =>
        false
      case 2 | 3 =>
        true
      case _ if value <= 0 => throw new IllegalArgumentException(NegativeValueErrorMessage)
      case _ =>
        val (r, d) = factorOutMultiplesOf2(m)
        (0 to Math.min(k, value)).forall { _ =>
          //a it's a random int between 2 and n-2
          val a = 2 + Random.nextInt(value - 3)
          val x = BigInt(a).modPow(d, value).toInt
          if (x == 1 || x == m) {
            true
          } else {
            matchR(r, x)
          }
        }
    }
  }

  /**
   * Euclide method for greatest common divisor.
   *
   * @param other
   * @return
   */
  def gcd(other: ArithmeticInt): ArithmeticInt = other match {
    case ArithmeticInt(0) => value
    case ArithmeticInt(v) => v.gcd(value % v)
  }

  /**
   *
   * @param other
   * @return
   */
  def isCoprime(other: ArithmeticInt): Boolean = Math.abs(this.gcd(other)) == 1

  /**
   * Compute Euler's totient function.
   * https://en.wikipedia.org/wiki/Euler's_totient_function
   *
   * @return
   */
  def totient(): Int = value match {
    case 0 => 0
    case i if i < 0  => Math.abs(i).totient()
    case _ =>
      1 + (2 to (value - 1)).map { i =>
       if (this.gcd(i) == 1) 1 else 0
      }.sum
  }

  def primeFactors(): Seq[ArithmeticInt] = value match {
    case 0|1 => Nil
    case i if i < 0  => Math.abs(i).primeFactors()
    case _ if this.isPrime() => Seq(this)
    case _ =>
      (2 to (value / 2)).view.map(IntToArithmeticInt).filter(n => n.isPrime && !this.isCoprime(n)).toSeq
  }

  /**
   * Returns prime factors for any Int, with their respective multiplicty.
   * @return
   */
  def primeFactorsMultiplicity(): Map[ArithmeticInt, ArithmeticInt] = {
    val largestPossibleFactor = value / 2
    def factorR(number: ArithmeticInt, nextFactor: ArithmeticInt): Map[ArithmeticInt, ArithmeticInt] = {
      if (nextFactor > largestPossibleFactor) {
        Map.empty
      } else if (nextFactor.isPrime && !number.isCoprime(nextFactor)) {
        val MultiplicityResult(recMultiplier, recDivisionResult) = multiplicity(number / nextFactor, nextFactor)
        factorR(recDivisionResult, nextFactor + 1) + ((nextFactor, recMultiplier + 1))
      } else {
        factorR(number, nextFactor + 1)
      }
    }

    value match {
      case 0|1 => Map.empty
      case i if i < 0  => Math.abs(i).primeFactorsMultiplicity()
      case _ if this.isPrime() => Map(this -> 1)
      case _ =>
        factorR(this, 2)
    }
  }

  /**
   * Compute Euler's totient function using a list of prime factors (with their multiplicity)
   * https://en.wikipedia.org/wiki/Euler's_totient_function
   *
   * @return
   */
  def totientFast(): Int = value match {
    case 0 => 0
    case i if i < 0  => Math.abs(i).totientFast()
    case _ =>
      this.primeFactorsMultiplicity().foldLeft(1){ case (accum, (factor, multiplicity)) =>
        accum * (factor - 1) * Math.pow(factor.toInt, multiplicity - 1).toInt
      }
  }

  @throws[IllegalArgumentException](NonDecreasingIndicesIllegalArgumentErrorMessage)
  def primesTo(j: Int): Seq[ArithmeticInt] = {
    def EulerCrivel(primes: Set[Int]): Set[Int] = {
      val current = Try(primes.min).getOrElse(j + 1)
      if (current > j) {
        primes
      } else {
        val remainingPrimes = primes -- current.to(j, current).toSet
        EulerCrivel(remainingPrimes) + current
      }
    }

    if (j < value) {
      throw new IllegalArgumentException(NonDecreasingIndicesIllegalArgumentErrorMessage)
    } else {
      EulerCrivel((2 to j).toSet).filter(_ >= value).toSeq.sorted.map(IntToArithmeticInt)
    }
  }

  /**
   * Compute a pair of prime numbers that sum up to any even number greater than 2.
   * https://en.wikipedia.org/wiki/Goldbach's_conjecture
   *
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](GoldbachIllegalArgumentException)
  def goldbachDecomposition(): (ArithmeticInt, ArithmeticInt) = value match {
    case i if i < 4 || i % 2 != 0 =>
      throw new IllegalArgumentException(GoldbachIllegalArgumentException)
    case _ =>
      val lowPrimes = 2.primesTo(value / 2)
      val highPrimes = (value / 2).primesTo(value).toSet
      val factor = lowPrimes.filter { i =>
        highPrimes.contains(value - i)
      }.head

      (factor, value - factor)
  }

  ////////////////////////////////////////////
  //              Utilities
  ////////////////////////////////////////////

  case class MultiplicityResult(multiplicity: Int, divisionResult: Int)
  
  private def multiplicity(dividend: Int, divisor: Int): MultiplicityResult = (dividend, divisor) match {
    case (_, 0) => MultiplicityResult(0, dividend)
    case _ if dividend % divisor != 0 => MultiplicityResult(0, dividend)
    case _ =>
      val MultiplicityResult(recMultiplier, recDivisionResult) = multiplicity(dividend / divisor, divisor)
      MultiplicityResult(1 + recMultiplier, recDivisionResult)
  }

  /**
   * Represent an int as the product 2**r * d
   * @param n
   * @return
   */
  private def factorOutMultiplesOf2(n: Int): (Int, Int) = {
    /**
     * @param i
     * @return Exponent of the largest power of 2 dividing i.
     */
    def p2(i:Int) = {
      val s = i.toBinaryString;
      s.size - 1 - s.lastIndexOf('1')
    }

    val r = p2(n)
    (r, n / (1 << r))
  }
}