package org.mlarocca.s99.graph

abstract class Graph[K, T](_vertices: Seq[Vertex[K, T]] = Nil, _edges: Seq[Edge[K, T]] = Nil) {
  lazy val vertices = _vertices.toSet
  lazy val edges = _edges.toSet

  protected lazy val verticesMap = vertices.map(v => v.key -> v).toMap

  def canEqual(other: Any): Boolean = other.isInstanceOf[Graph[K, T]]

  override def equals(other: Any): Boolean = other match {
    case that: Graph[_, _] =>
      (that canEqual this) &&
      hashCode() == that.hashCode()
    case _ =>
      false
  }

  override def hashCode(): Int = (vertices ++ edges).map(_.hashCode()).hashCode()

  def hasVertex(v: Vertex[K, T]): Boolean = vertices.contains(v)
  def hasVertex(key: K): Boolean = verticesMap.contains(key)
  def getVertex(key: K): Option[Vertex[K, T]] = verticesMap.get(key)

  def hasEdge(e: Edge[K, T]): Boolean = edges.contains(e)
  def hasEdge(u: Vertex[K, T], v: Vertex[K, T]): Boolean = edges.exists{ e =>
    e.source == u && e.destination == v
  }

  def edgesFrom(label: K): Set[Edge[K, T]] = getVertex(label).map(_.adj).getOrElse(Nil).toSet
  def edgesTo(label: K): Set[Edge[K, T]] = {
    getVertex(label).map { v =>
      edges.filter(_.destination == v)
    }.getOrElse(Set.empty)
  }

  def edgesTo(v: Vertex[K, T]): Set[Edge[K, T]] = edges.toSet.filter(_.destination == v)

  @throws[IllegalArgumentException]
  def addVertex[J >: K, U >: T](label: J): Graph[J, U] = ???

  @throws[IllegalArgumentException]
  def addVertex[J >: K, U >: T](v: Vertex[J, U]): Graph[J, U] = ???

  @throws[IllegalArgumentException]
  def addEdge[J >: K, U >: T](e: Edge[J, U]): Graph[J, U] = ???
}

object Graph {
  private[graph] val DuplicateVertexExceptionMessage = "The Graph already has a Vertex %s"
}