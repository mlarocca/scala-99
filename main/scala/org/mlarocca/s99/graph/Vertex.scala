package org.mlarocca.s99.graph

abstract case class Vertex[+K, +T](val key: K, val adj: Seq[Edge[K, T]] = Nil) {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Vertex[K, T]]

  override def equals(other: Any): Boolean = other match {
    case that: Vertex[_, _] => (that canEqual this) && this.hashCode() == other.hashCode()
    case _ => false
  }

  override def hashCode(): Int = s"${key.toString}[${key.getClass}}]".hashCode

  override def toString(): String = s"v[$key]"

  def neighbors[J >: K, U >: T]: Set[Vertex[J, U]] = adj.map(_.destination).toSet
}
