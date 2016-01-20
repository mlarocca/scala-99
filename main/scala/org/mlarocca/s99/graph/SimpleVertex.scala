package org.mlarocca.s99.graph

class SimpleVertex[+K <% Ordered[K], +T](
    override val key: K,
    override val adj: Seq[WeightedEdge[K, T]]= Nil) extends Vertex[K, T](key, adj) {
}

object SimpleVertex {
  def apply[K <% Ordered[K]](key: K) = new SimpleVertex[K, String](key)
}
