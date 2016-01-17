package org.mlarocca.s99.graph

class LabeledEdge[+K](
    override val source: SimpleVertex[K],
    override val destination: SimpleVertex[K],
    val label: String) extends Edge[K](source, destination) {

  override def canEqual(other: Any): Boolean = {
    lazy val thatEdge = other.asInstanceOf[LabeledEdge[K]]

    other.isInstanceOf[LabeledEdge[K]] &&
        source.canEqual(thatEdge.source) &&
        destination.canEqual(thatEdge.destination)
  }

  override def hashCode(): Int = (source.hashCode(), destination.hashCode(), label).hashCode()
}