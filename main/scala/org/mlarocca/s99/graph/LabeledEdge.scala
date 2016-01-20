package org.mlarocca.s99.graph

class LabeledEdge[+K <% Ordered[K], +T](
    override val source: K,
    override val destination: K,
    val label: T) extends Edge[K, T](source, destination) {

  override def canEqual(other: Any): Boolean = {
    lazy val thatEdge = other.asInstanceOf[LabeledEdge[K, T]]

    other.isInstanceOf[LabeledEdge[K, T]]
  }

  override def hashCode(): Int = (source.hashCode(), destination.hashCode(), label).hashCode()

  override def toString(): String = s"e($label)[$source -> $destination]"
}

object LabeledEdge {
  def apply[K <% Ordered[K]](source: K, dest: K) = new LabeledEdge[K, String](source, dest, "")
}