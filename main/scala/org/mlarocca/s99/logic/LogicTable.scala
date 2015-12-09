package org.mlarocca.s99.logic

object LogicTable {
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
