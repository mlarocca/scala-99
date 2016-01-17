package org.mlarocca.s99.graph

class WeightedEdge[+K, +T](
    override val source: SimpleVertex[K, T],
    override val destination: SimpleVertex[K, T],
    override val label: T,
    val weight: Double) extends LabeledEdge[K, T](source, destination, label) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge[K, T]]

  override def toString(): String = s"${super.toString()}{$weight}"

  lazy val hasNegativeWeight = weight < 0
}

object WeightedEdge {
  def apply[K](source: SimpleVertex[K, String], dest: SimpleVertex[K, String]) = new WeightedEdge[K, String](source, dest, "", 0)
  def apply[K, T](source: SimpleVertex[K, T], dest: SimpleVertex[K, T], label: T) = new WeightedEdge[K, T](source, dest, label, 0)

  implicit def labeled2WeightedEdge[K, T](e: LabeledEdge[K, T]): WeightedEdge[K, T] = WeightedEdge(e.source, e.destination, e.label)
}