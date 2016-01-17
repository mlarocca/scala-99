package org.mlarocca.s99.graph

class SimpleVertex[+K](
    override val key: K,
    override val adj: Seq[WeightedEdge[K]]= Nil) extends Vertex[K](key, adj) {
}

object SimpleVertex {
  def apply[K](key: K) = new SimpleVertex[K](key)
}
