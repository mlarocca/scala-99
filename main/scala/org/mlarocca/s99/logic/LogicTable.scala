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
}

class LogicTable {

}
