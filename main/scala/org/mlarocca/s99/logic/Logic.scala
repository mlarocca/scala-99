package org.mlarocca.s99.logic

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap}

object LogicTable {

  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def not(a: Boolean): Boolean = a match {
    case false => true
    case _ => false
  }

  def equ(a: Boolean, b: Boolean): Boolean = {
    or(and(a, b), and(not(a), not(b)))
  }

  def xor(a: Boolean, b: Boolean): Boolean = {
    not(equ(a, b))
  }

  def nor(a: Boolean, b: Boolean): Boolean = {
    not(or(a, b))
  }

  def nand(a: Boolean, b: Boolean): Boolean = {
    not(and(a, b))
  }

  def impl(a: Boolean, b: Boolean): Boolean = {
    or(not(a), b)
  }

  implicit def LogicTableToBoolean(a: LogicTable): Boolean = a.value
  implicit def BooleanToLogicTable(a: Boolean): LogicTable = LogicTable(a)
}

private object Codes {

  @throws[IllegalArgumentException]
  val gray: (Int) => Seq[String] = memoizer({
    case n: Int if (n <= 0) =>
      throw new IllegalArgumentException(NegativeValueErrorMessage)
    case n: Int =>
      val base = gray(n - 1)
      base.map("0" + _) ++ base.reverse.map("1" + _)
  },
  MutableMap(1 -> Seq("0", "1"))
  )

  /**
   * Computes the Huffman code of a sequence of symbols, based on their frequencies.
   *
   * @param symbolsFrequencies A sequence of Tuples: each symbol paired with its frequency.
   * @tparam V The type of the input symbols.
   * @return A list,of the symbols, in the same order as they appear in the input, paired with their Huffman encoding,
   */
  def huffman[V](symbolsFrequencies: Seq[(V, Int)]): Seq[(V, String)] = symbolsFrequencies match {
    case Nil =>
      Nil

    case (s, _) :: Nil =>
      Seq((s, "0"))

    case _ =>

      def updateMap(symbol: String)(accumMap: Map[V, String], pattern: V): Map[V, String] = {
        val prevString = accumMap(pattern)
        accumMap + ((pattern, prevString + symbol))
      }

      def huffmanR(frequenciesQueue: mutable.PriorityQueue[QueueElement[V]]): Map[V, String] = frequenciesQueue.size match {
        case 0|1 => Map.empty.withDefaultValue("")
        case _ =>
          val e1 = frequenciesQueue.dequeue()
          val e2 = frequenciesQueue.dequeue()
          val newEl = QueueElement[V](e1.value ++ e2.value, e1.priority + e2.priority)

          val leftBranchMap = e2.value.foldLeft(huffmanR(frequenciesQueue.+=(newEl)))(updateMap("1"))
          e1.value.foldLeft(leftBranchMap)(updateMap("0"))
      }

      val symbolsTable = huffmanR(
        symbolsFrequencies.foldLeft(
          new mutable.PriorityQueue[QueueElement[V]]()(Ordering.by((_: QueueElement[V]).priority).reverse)
        ){ (queue, f) =>
          queue.+=(QueueElement(Seq(f._1), f._2))
        }
      )

      symbolsFrequencies.map {
        case (s, _) => (s, symbolsTable(s))
      }
  }

  private[s99] val NegativeValueErrorMessage = "Value must be positive"
  private case class QueueElement[V](value: Seq[V], priority: Int)

  /**
   * Turn a function f into a memoized function, checking local cache before computing its value.
   *
   * @param f
   * @param cache
   * @return
   */
  private def memoizer(
      f: Int => Seq[String],
      cache: MutableMap[Int, Seq[String]] = MutableMap.empty
      ): Int => Seq[String] = {

    (i: Int) => {
      if (!cache.contains(i)) {
        cache.+=((i, f(i)))
      }
      cache(i)
    }
  }
}

case class LogicTable(value:  Boolean) {

  override def hashCode = if (value) 1 else 0

  override def canEqual(other: Any): Boolean = {
    other.isInstanceOf[LogicTable]
  }

  override def equals(other: Any) = other match {
    case thatValue: Boolean => this.value == thatValue
    case that: LogicTable => (that canEqual this) && this.hashCode == that.hashCode
    case _ => false
  }

  def and(b: LogicTable):LogicTable = LogicTable.and(this, b)

  def or(b: LogicTable):LogicTable = LogicTable.or(this, b)

  def equ(b: LogicTable):LogicTable = LogicTable.equ(this, b)

  def xor(b: LogicTable):LogicTable = LogicTable.xor(this, b)

  def nor(b: LogicTable):LogicTable = LogicTable.nor(this, b)

  def nand(b: LogicTable):LogicTable = LogicTable.nand(this, b)

  def impl(b: LogicTable):LogicTable = LogicTable.impl(this, b)

  def not():LogicTable = LogicTable.not(this)
}