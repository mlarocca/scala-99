package org.mlarocca.s99.graph

abstract case class Edge[+K](val source: Vertex[K], val destination: Vertex[K]) {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Edge[K]]

  override def equals(other: Any): Boolean = other match {
    case that: Edge[_] => (that canEqual this) && this.hashCode() == other.hashCode()
    case _ => false
  }

  override def hashCode(): Int = (source.hashCode(), destination.hashCode()).hashCode()
}
