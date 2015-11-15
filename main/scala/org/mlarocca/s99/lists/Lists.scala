package com.mlarocca.s99.lists

import java.util.NoSuchElementException

object Utils {
  private val TailErrorMessage = "Input sequence can't be empty"
  private val PenultimateErrorMessage = "Input sequence needs to have at least 2 elements"
  private val NthErrorMessage = "Input sequence doesn't have enough elements"
  private val NthIllegalArgumentErrorMessage = "Index must be a non-negative integer"

  /**
   *
   * @param s
   * @tparam T
   * @throws NoSuchElementException
   * @return
   */
  @throws[NoSuchElementException](TailErrorMessage)
  def last[T](s: Seq[T]): T = s match {
    case Nil => throw new NoSuchElementException(TailErrorMessage)
    case x::Nil => x
    case _::xs => last(xs)
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def lastOption[T](s: Seq[T]): Option[T] = {
    try {
      Some(last(s))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  /**
   *
   * @param s
   * @tparam T
   * @throws NoSuchElementException
   * @return
   */
  @throws[NoSuchElementException](PenultimateErrorMessage)
  def penultimate[T](s: Seq[T]): T = s match {
    case Nil|_::Nil => throw new NoSuchElementException(PenultimateErrorMessage)
    case x::_::Nil => x
    case _::xs => penultimate(xs)
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def penultimateOption[T](s: Seq[T]): Option[T] = {
    try {
      Some(penultimate(s))
    } catch {
      case _: NoSuchElementException => None
    }
  }

  /**
   *
   * @param n Position of the element in the sequence - 0-based.
   * @param s
   * @tparam T
   * @throws NoSuchElementException
   * @return
   */
  @throws[NoSuchElementException](NthErrorMessage)
  @throws[IllegalArgumentException](NthIllegalArgumentErrorMessage)
  def nth[T](n: Int, s: Seq[T]): T = (n, s) match {
    case (i, _) if i < 0 => throw new IllegalArgumentException(NthIllegalArgumentErrorMessage)
    case (_, Nil) => throw new NoSuchElementException(NthErrorMessage)
    case (0, _) => s.head
    case (_, x::xs) => nth(n-1, xs)
  }

  /**
   *
   * @param n Position of the element in the sequence - 0-based.
   * @param s
   * @tparam T
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](NthErrorMessage)
  def nthOption[T](n: Int, s: Seq[T]) = {
    try {
      Some(nth(n, s))
    } catch {
      case e: NoSuchElementException => None
    }
  }

  /**
   *
   * @param s
   * @return
   */
  def length(s: Seq[_]): Int = s match {
    case Nil => 0
    case x::xs => 1 + length(xs)
  }

  /**
   *
   * @param s
   * @return
   */
  def reverse(s: Seq[_]): Seq[_] = {
    def reverseCarryOver(s1: Seq[_], s2: Seq[_]): Seq[_] = s1 match {
      case Nil => s2
      case x::xs => reverseCarryOver(xs, x +: s2)
    }

    reverseCarryOver(s, Nil)
  }

  /**
   *
   * @param s
   * @return
   */
  def isPalindrome(s: Seq[_]): Boolean = {
    def isPalindromeAccum(left: Seq[_], right: Seq[_]): Boolean = right match {
      case _ if length(left) == length(right) => left == right
      case x::xs if length(left) == length(right) - 1 => left  == xs
      case x::xs => isPalindromeAccum(x +: left, xs)
    }
    isPalindromeAccum(Nil, s)
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def flatten[T](s: Seq[_]): Seq[_] = s match {
    case Nil => Nil
    case x::xs => x match {
      case ys: Seq[_] => flatten(ys) ++ flatten(xs)
      case _ => x +: flatten(xs)
    }
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def compress[T](s: Seq[T]): Seq[T] = s match {
    case Nil => Nil
    case _::Nil => s
    case x::(xs@y::ys) if x == y => compress(xs)
    case x::xs => x +: compress(xs)
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def pack[T](s: Seq[T]): Seq[Seq[T]] = {
    def packRecursive[T](list: Seq[T], subList: Seq[T]): Seq[Seq[T]] = {
      val lastEl = subList.head
      list match {
        case Nil => Seq(subList)
        case x :: xs if x == lastEl => packRecursive(xs, x +: subList)
        case x :: xs => subList +: packRecursive(xs, Seq(x))
      }
    }

    s match {
      case Nil => Nil
      case x::xs => packRecursive(xs, Seq(x))
    }
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def encode[T](s: Seq[T]): Seq[(T, Int)] = {
    pack(s).map { subList =>
      (subList.head, length(subList))
    }
  }

  /**
   *
   * @param s
   * @tparam T
   * @return
   */
  def encodeModified[T](s: Seq[T]) = {
    pack(s).map { subList =>
      if (subList.length == 1) {
        subList.head
      } else {
        (subList.head, length(subList))
      }
    }
  }
}
