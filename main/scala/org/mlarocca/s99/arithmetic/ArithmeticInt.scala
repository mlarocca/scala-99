package org.mlarocca.s99.arithmetic

import scala.annotation.tailrec
import scala.util.Random


object ArithmeticInt {
  implicit def IntToArithmeticInt(i: Int): ArithmeticInt = ArithmeticInt(i)
  implicit def ArithmeticIntToInt(ai: ArithmeticInt): Int = ai.value

  private[s99] val NegativeValueErrorMessage = "Value must be positive"
}

case class ArithmeticInt(value: Int) {
  import ArithmeticInt._

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
   * @param k How many iterations should be run. Precision ~= 2^(-k).
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException]
  def isProbablyPrime(k: Int = 100): Boolean = {
    val m = value - 1

    /**
     * Scala translation for:
     *  repeat r − 1 times:
     *    x ← x^2 mod n
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
   * Represent an int as the product 2^r * d
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