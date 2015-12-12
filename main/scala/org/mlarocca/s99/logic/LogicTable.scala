package org.mlarocca.s99.logic

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap}

object LogicTable {
  private[s99] val NegativeValueErrorMessage = "Value must be positive"

  def and(a: Boolean, b: Boolean): Boolean = (a,b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a,b) match {
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

  private def memoizer(
    f: Int => Seq[String],
    cache: MutableMap[Int, Seq[String]] = MutableMap.empty
  ): Int => Seq[String] = {

    (i: Int) => {
      if (!cache.contains(i)) {
        cache.+=((i, f(i)))
        println(cache)
      }
      cache(i)
    }
  }

  @throws[IllegalArgumentException]
  val gray: (Int) => Seq[String] = memoizer({
      case n: Int if (n <= 0) =>
          throw new IllegalArgumentException(NegativeValueErrorMessage)
      case n: Int =>
        println(s"computing grayR($n)")
        val base = gray(n - 1)
        base.map("0" + _) ++ base.reverse.map("1" + _)
    },
    MutableMap(1 -> Seq("0", "1"))
  )

  private case class QueueElement(value: Seq[String], priority: Int)

  def huffman(frequencies: Seq[(String, Int)]): Seq[(String, String)] = {
    def huffmanR(frequenciesQueue: mutable.PriorityQueue[QueueElement]): Map[String, String] = frequenciesQueue.size match {
      case 0 => Map.empty
      case 1 => frequenciesQueue.head.value.map{ (_ -> "") }.toMap.withDefaultValue("")
      case _ =>
        val e1 = frequenciesQueue.dequeue()
        val e2 = frequenciesQueue.dequeue()
        val newEl = QueueElement(e1.value ++ e2.value, e1.priority + e2.priority)
        frequenciesQueue.+=(newEl)
        val lMap = e2.value.foldLeft(huffmanR(frequenciesQueue)) {
          case (hMap, s) =>
            val prevString = hMap(s)
            hMap + ((s, prevString + "1"))
        }
        e1.value.foldLeft(lMap) {
          case (hMap, s) =>
            val prevString = hMap(s)
            hMap + ((s, prevString + "0"))
        }

    }

    val pq = new mutable.PriorityQueue[QueueElement]()(Ordering.by((_: QueueElement).priority).reverse)
    val symbolsTable = huffmanR(frequencies.foldLeft(pq){ (queue, f) =>
      pq.+=(QueueElement(Seq(f._1), f._2))
    })

    frequencies.map {
      case (s, _) => (s, symbolsTable(s))
    }
  }

  implicit def LogicTableToBoolean(a: LogicTable): Boolean = a.value
  implicit def BooleanToLogicTable(a: Boolean): LogicTable = LogicTable(a)
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
