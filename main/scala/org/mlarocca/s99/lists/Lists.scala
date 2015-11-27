package com.mlarocca.s99.lists

import java.util.NoSuchElementException
import scala.util.Random

object Utils {
  private[s99] val TailErrorMessage = "Input sequence can't be empty"
  private[s99] val PenultimateErrorMessage = "Input sequence needs to have at least 2 elements"
  private[s99] val NthErrorMessage = "Input sequence doesn't have enough elements"
  private[s99] val NthIllegalArgumentErrorMessage = "Index must be a non-negative integer"
  private[s99] val DuplicateNIllegalArgumentErrorMessage = "The multiplier factor must be a non-negative integer"
  private[s99] val NegativeMultiplierFactorIllegalArgumentErrorMessage = "Multipliers must be non-negative"
  private[s99] val NegativeSizellegalArgumentErrorMessage = "n must be positive"
  private[s99] val InvalidCounterllegalArgumentErrorMessage = "n must be non-negative and not greather than the size of the sequence"
  private[s99] val IndexOutOfBoundsIllegalArgumentErrorMessage = "Valid indices are 0 <= i <= s.length"
  private[s99] val NonDecreasingIndicesIllegalArgumentErrorMessage = "Indices must be in non-decreasing order"

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @return
   */
  def length(s: Seq[_]): Int = s match {
    case Nil => 0
    case x::xs => 1 + length(xs)
  }

  /**
   *
   * @param s The input sequence
   * @return
   */
  def reverse[T](s: Seq[T]): Seq[T] = {
    def reverseCarryOver(s1: Seq[T], s2: Seq[T]): Seq[T] = s1 match {
      case Nil => s2
      case x::xs => reverseCarryOver(xs, x +: s2)
    }

    reverseCarryOver(s, Nil)
  }

  /**
   *
   * @param s The input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @return
   */
  def encode[T](s: Seq[T]): Seq[(T, Int)] = {
    pack(s).map { subList =>
      (subList.head, length(subList))
    }
  }

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
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

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @return
   * @throws IllegalArgumentException
   */
  @throws[IllegalArgumentException](NegativeMultiplierFactorIllegalArgumentErrorMessage)
  def decode[T](s: Seq[(T, Int)]): Seq[T] = {
    def expand(e: T, n: Int): Seq[T] = n match {
      case 0 => Nil
      case i if i < 0 => throw new IllegalArgumentException(NegativeMultiplierFactorIllegalArgumentErrorMessage)
      case _ => e +: expand(e, n-1)
    }

    flatten(s.map{ case (e, i: Int) => expand(e, i)}).asInstanceOf[Seq[T]]
  }

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @return
   */
  def encodeDirect[T](s: Seq[T]): Seq[(T, Int)] = {
    def encodecursive[T](list: Seq[T], e: T, n: Int): Seq[(T, Int)] = {
      list match {
        case Nil => Seq((e, n))
        case x :: xs if x == e => encodecursive(xs, x, n + 1)
        case x :: xs => (e, n) +: encodecursive(xs, x, 1)
      }
    }

    s match {
      case Nil => Nil
      case x::xs => encodecursive(xs, x, 1)
    }
  }

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @return
   */
  def duplicate[T](s: Seq[T]): Seq[T] = {
    decode(s.map((_, 2)))
  }

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @return
   */
  def duplicateDirect[T](s: Seq[T]): Seq[T] = {
    s.foldRight(Seq.empty[T]) { case (x, xs) =>
      x +: x +: xs
    }
  }

  /**
   *
   * @param n
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](DuplicateNIllegalArgumentErrorMessage)
  def duplicateN[T](n: Int, s: Seq[T]): Seq[T] = n match {
    case 0 => Nil
    case _ if n < 0 => throw new IllegalArgumentException(DuplicateNIllegalArgumentErrorMessage)
    case _ => decode(s.map((_, n)))
  }

  /**
   *
   * @param n
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](NegativeSizellegalArgumentErrorMessage)
  def dropN[T](n: Int, s: Seq[T]): Seq[T] = {
    val m = length(s)
    def doDropN(i: Int, left: Seq[T]): Seq[T] = (i, left) match {
      case (_, Nil) => Nil
      //INVARIANT: `i` can't be non positive
      case (1, x::xs) => doDropN(n, xs)
      case (_, _) if i > m => s
      case (_, x::xs) => x +: doDropN(i - 1, xs)
    }
    if (n <= 0) {
      throw new IllegalArgumentException(NegativeSizellegalArgumentErrorMessage)
    } else {
      doDropN(n, s)
    }
  }

  /**
   *
   * @param n
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](NegativeSizellegalArgumentErrorMessage)
  def split[T](n: Int, s: Seq[T]): (Seq[T], Seq[T]) = {
    val m = length(s)

    def doSplit(size: Int, left: Seq[T], right: Seq[T]): (Seq[T], Seq[T]) = (size, left, right) match {
      //INVARIANT: `size` can't be non positive nor bigger than s' size, so right can't be Nil unless size == 0
      case (0, _, _) => (reverse(left), right)
      case (_, _, x::xs) => doSplit(size - 1, x+:left, xs)
    }

    if (n < 0 || n > length(s)) {
      throw new IndexOutOfBoundsException(NegativeSizellegalArgumentErrorMessage)
    } else {
      doSplit(n, Nil, s)
    }
  }

  /**
   *
   * @param i
   * @param j
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](NthIllegalArgumentErrorMessage)
  def slice[T](i: Int, j: Int, s: Seq[T]): Seq[T] = {
    val splitAtI = split(i, s)
    split(j - i, splitAtI._2)._1
  }

  /**
   *
   * @param i
   * @param j
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](NthIllegalArgumentErrorMessage)
  def sliceDirect[T](i: Int, j: Int, s: Seq[T]): Seq[T] = {
    def doDrop(size: Int, left: Seq[T]): Seq[T] = (size, left) match {
      case (0, _) => left
      //INVARIANT: `size` can't be non positive nor bigger than s' size, so right can't be Nil unless size == 0
      case (_, x::xs) => doDrop(size - 1, xs)
    }

    def doTake(size: Int, left: Seq[T]): Seq[T] = (size, left) match {
      //INVARIANT: `i` can't be non positive nor bigger than s' size, so right can't be Nil unless size == 0
      case (0, _) => Nil
      case (_, x::xs) => x +: doTake(size - 1, xs)
    }
    if (i < 0 || j > length(s) || i > j) {
      throw new IndexOutOfBoundsException(IndexOutOfBoundsIllegalArgumentErrorMessage)
    }

    doTake(j - i, doDrop(i, s))
  }

  /**
   *
   * @param n
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](NegativeSizellegalArgumentErrorMessage)
  def rotate[T](n: Int, s: Seq[T]): Seq[T] = {
    val (left, right) = split(n, s)
    right ++ left
  }

  /**
   *
   * @param i The index of element to remove from the list
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](IndexOutOfBoundsIllegalArgumentErrorMessage)
  def removeAt[T](i: Int, s: Seq[T]): (Seq[T], T) = {
    def doRemove(j: Int, left: Seq[T]): (Seq[T], T) = {
      //Invariant: left can't be empty
      val x::xs = left
      if (j == 0) {
        (xs, x)
      } else {
        val (restoOfList, removedEl) = doRemove(j - 1, xs)
        (x +: restoOfList, removedEl)
      }
    }
    
    if (i < 0 || i >= length(s)) {
      throw new IndexOutOfBoundsException(IndexOutOfBoundsIllegalArgumentErrorMessage)
    } else {
      doRemove(i, s)
    }
  }

  /**
   *
   * @param elem The new element to add to the list
   * @param i The element to remove from the list
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](IndexOutOfBoundsIllegalArgumentErrorMessage)
  def insertAt[T](elem: T, i: Int, s: Seq[T]): Seq[T] = {
    try {
      val (left, right) = split(i, s)
      left ++ (elem +: right)
    } catch {
      case e: IndexOutOfBoundsException if e.getMessage == NegativeSizellegalArgumentErrorMessage =>
        throw new IndexOutOfBoundsException(IndexOutOfBoundsIllegalArgumentErrorMessage)
    }
  }

  /**
   *
   * @param elem The new element to add to the list
   * @param i The element to remove from the list
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IndexOutOfBoundsException](IndexOutOfBoundsIllegalArgumentErrorMessage)
  def insertAtDirect[T](elem: T, i: Int, s: Seq[T]): Seq[T] = {
    def doInsertAt(j: Int, left: Seq[T]): Seq[T] = j match {
      case 0 => elem +: left
      case _ =>
        //INVARIANT: left can't be empty unless j == 0
        val x::xs = left
        x +: doInsertAt(j - 1, xs)
    }

    if (i < 0 || i > length(s)) {
      throw new IndexOutOfBoundsException(IndexOutOfBoundsIllegalArgumentErrorMessage)
    } else {
      doInsertAt(i, s)
    }
  }

  /**
   * Return the range from i included to j excluded
   *
   * @param i First index
   * @param j Second index
   * @throws IllegalArgumentException if j < i
   * @return
   */
  @throws[IllegalArgumentException](NonDecreasingIndicesIllegalArgumentErrorMessage)
  def range(i: Int, j: Int): Seq[Int] = {
    def rangeR(k: Int, s: Seq[Int]): Seq[Int] = k match {
      case _ if k == i => s
      case _ if i < k =>
        val z = k - 1
        rangeR(z, z +: s)
    }

    if (j < i) {
      throw new IllegalArgumentException(NonDecreasingIndicesIllegalArgumentErrorMessage)
    } else {
      rangeR(j, Nil)
    }
  }

  /**
   *
   * @param n The number of elements to be extracted from the list
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](InvalidCounterllegalArgumentErrorMessage)
  def randomSelect[T](n: Int, s: Seq[T]): Seq[T] = {
    /**
     * Tail recursive version
     *
     * @param m How many elements we still need to extract
     * @param left What's left of s after extracting n - m elements
     * @param selected The elements randomly sected so far
     * @return
     */
    def randomSelectR(m: Int, left: Seq[T], selected: Seq[T]): Seq[T] = m match {
      //Invariant m >= 0
      case 0 => selected
      case _ =>
        val (rest, el) = removeAt(Random.nextInt(length(left)), left)
        randomSelectR(m - 1, rest, el +: selected)
    }

    if (n < 0) {
      throw new IllegalArgumentException(InvalidCounterllegalArgumentErrorMessage)
    } else {
      randomSelectR(n, s, Nil)
    }
  }

  /**
   *
   * @param extracted The number of elements to be extracted from the list
   * @param maxValue The upper bound for the range
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](InvalidCounterllegalArgumentErrorMessage)
  @throws[IllegalArgumentException](NonDecreasingIndicesIllegalArgumentErrorMessage)
  def lotto(extracted: Int, maxValue: Int): Seq[Int] = {
    randomSelect(extracted, range(1, maxValue + 1))
  }

  /**
   *
   * @param k The size of the subgroups
   * @throws IllegalArgumentException
   * @return
   */
  @throws[IllegalArgumentException](InvalidCounterllegalArgumentErrorMessage)
  def combinations[T](k: Int, s: Seq[T]): Seq[Seq[T]] = {

    def goThroughList(s: Seq[T]): Seq[Seq[T]] = s match {
      case Nil => Nil
      case x :: xs =>
        val xCombinations = combinations(k - 1, xs).map {
          x +: _
        }
        val recursive = goThroughList(xs)
        xCombinations ++ recursive
    }

    k match {
      case _ if k < 0 => throw new IllegalArgumentException(InvalidCounterllegalArgumentErrorMessage)
      case _ if k == 0 || k > length(s) => Nil
      case 1 => s.map(Seq(_))
      case _ => goThroughList(s)
    }

  }

  /**
   *
   * @param s The input sequence
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  def randomPermute[T](s: Seq[T]): Seq[T] = {
    randomSelect(length(s), s)
  }

  /**
   *
   * Sort a list of the second order according to the sublists' length.
   *
   * @param s The input sequence (a sequence of sequences)
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  def lsort[T](s: Seq[Seq[T]]): Seq[Seq[T]] = s.sorted(new Ordering[Seq[T]]() {
    override def compare(xs: Seq[T], ys: Seq[T]): Int = xs.size - ys.size
  })

  /**
   * Sort a list of the second order according to the frequency of the length of each sublist.
   *
   * @param s The input sequence (a sequence of sequences)
   * @tparam T The generic type of elements in the input sequence
   * @throws IllegalArgumentException
   * @return
   */
  def lsortFreq[T](s: Seq[Seq[T]]): Seq[Seq[T]] = {
    val lFreq = s.groupBy(xs => length(xs)).map{ case (size, xss) => size -> length(xss)}
    s.sorted(new Ordering[Seq[T]]() {
      override def compare(xs: Seq[T], ys: Seq[T]): Int = lFreq(xs.size) - lFreq(ys.size)
    })
  }
}
