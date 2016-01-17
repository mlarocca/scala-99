package org.mlarocca.s99.graph

class LabeledEdge[+K, +T](
    override val source: SimpleVertex[K, T],
    override val destination: SimpleVertex[K, T],
    val label: T) extends Edge[K, T](source, destination) {

  override def canEqual(other: Any): Boolean = {
    lazy val thatEdge = other.asInstanceOf[LabeledEdge[K, T]]

    other.isInstanceOf[LabeledEdge[K, T]] &&
        source.canEqual(thatEdge.source) &&
        destination.canEqual(thatEdge.destination)
  }

  override def hashCode(): Int = (source.hashCode(), destination.hashCode(), label).hashCode()

  override def toString(): String = s"e($label)[$source -> $destination]"
}

object LabeledEdge {
  def apply[K](source: SimpleVertex[K, String], dest: SimpleVertex[K, String]) = new LabeledEdge[K, String](source, dest, "")
}