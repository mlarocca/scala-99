package org.mlarocca.s99.graph

abstract class Graph[K](_vertices: Seq[Vertex[K]] = Nil, _edges: Seq[Edge[K]] = Nil) {
  val vertices = _vertices.toSet
  val edges = _edges.toSet

  protected lazy val verticesMap = vertices.map(v => v.key -> v).toMap

  def hasVertex(v: Vertex[K]): Boolean = vertices.contains(v)
  def hasVertex(label: K): Boolean = verticesMap.contains(label)
  def getVertex(label: K): Option[Vertex[K]] = verticesMap.get(label)

  def hasEdge(e: Edge[K]): Boolean = edges.contains(e)
  def hasEdge(u: Vertex[K], v: Vertex[K]): Boolean = edges.exists{ e =>
    e.source == u && e.destination == v
  }

  def edgesFrom(label: K): Set[Edge[K]] = getVertex(label).map(_.adj).getOrElse(Nil).toSet
  def edgesTo(label: K): Set[Edge[K]] = {
    getVertex(label).map { v =>
      edges.filter(_.destination == v)
    }.getOrElse(Set.empty)
  }

  def edgesTo(v: Vertex[K]): Set[Edge[K]] = edges.toSet.filter(_.destination == v)

  @throws[IllegalArgumentException]
  def addVertex[T >: K](label: T): Graph[T]

}

object Graph {
  private[graph] val DuplicateVertexExceptionMessage = "The Graph already has a Vertex %s"
}