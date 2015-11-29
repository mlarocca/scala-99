package org.mlarocca.s99.arithmetic


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
}