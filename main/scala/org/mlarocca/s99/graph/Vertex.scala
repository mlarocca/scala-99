package org.mlarocca.s99.graph

abstract case class Vertex[+K](key: K, val adj: Seq[Edge[K]] = Nil) {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Vertex[K]]

  override def equals(other: Any): Boolean = other match {
    case that: Vertex[_] => (that canEqual this) && this.hashCode() == other.hashCode()
    case _ => false
  }

  override def hashCode(): Int = s"${key.toString}[${key.getClass}}]".hashCode

  def neighbors[T >:K]: Set[Vertex[T]] = adj.map(_.destination).toSet
}
