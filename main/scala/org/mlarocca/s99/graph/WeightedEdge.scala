package org.mlarocca.s99.graph

class WeightedEdge[+K](
    override val source: SimpleVertex[K],
    override val destination: SimpleVertex[K],
    override val label: String,
    val weight: Double) extends LabeledEdge[K](source, destination, label) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge[K]]

  lazy val hasNegativeWeight = weight < 0
}

object WeightedEdge {
  def apply[K](source: SimpleVertex[K], dest: SimpleVertex[K]) = new WeightedEdge[K](source, dest, "", 0)
  def apply[K](source: SimpleVertex[K], dest: SimpleVertex[K], label: String) = new WeightedEdge[K](source, dest, label, 0)
}