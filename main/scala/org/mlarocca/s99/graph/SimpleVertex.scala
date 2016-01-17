package org.mlarocca.s99.graph

class SimpleVertex[+K, +T](
    override val key: K,
    override val adj: Seq[WeightedEdge[K, T]]= Nil) extends Vertex[K, T](key, adj) {
}

object SimpleVertex {
  def apply[K](key: K) = new SimpleVertex[K, String](key)
}
