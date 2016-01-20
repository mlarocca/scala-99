package org.mlarocca.s99.graph

class WeightedEdge[+K <% Ordered[K], +T](
    override val source: K,
    override val destination: K,
    override val label: T,
    val weight: Double) extends LabeledEdge[K, T](source, destination, label) {

  override def canEqual(other: Any): Boolean = other.isInstanceOf[WeightedEdge[K, T]]

  override def toString(): String = s"${super.toString()}{$weight}"

  lazy val hasNegativeWeight = weight < 0
}

object WeightedEdge {
  def apply[K <% Ordered[K]](source: K, dest: K) = new WeightedEdge[K, String](source, dest, "", 0)
  def apply[K <% Ordered[K], T](source: K, dest: K, label: T) = new WeightedEdge[K, T](source, dest, label, 0)

  implicit def labeled2WeightedEdge[K <% Ordered[K], T](e: LabeledEdge[K, T]): WeightedEdge[K, T] = WeightedEdge(e.source, e.destination, e.label)
}