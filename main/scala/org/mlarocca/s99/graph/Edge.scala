package org.mlarocca.s99.graph

abstract case class Edge[+K <% Ordered[K], +T](val source: K, val destination: K) {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Edge[K, T]]

  override def equals(other: Any): Boolean = other match {
    case that: Edge[_, _] => (that canEqual this) && this.hashCode() == other.hashCode()
    case _ => false
  }

  override def hashCode(): Int = (source.hashCode(), destination.hashCode()).hashCode()

  override def toString(): String = s"e[$source -> $destination]"
}
