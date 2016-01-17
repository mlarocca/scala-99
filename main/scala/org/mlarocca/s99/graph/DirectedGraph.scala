package org.mlarocca.s99.graph

import scala.util.control.Exception._

class DirectedGraph[K, T](
    _vertices: Seq[SimpleVertex[K, T]] = Nil,
    _edges: Seq[WeightedEdge[K, T]] = Nil) extends Graph[K, T](
  _vertices, _edges) {

  @throws[IllegalArgumentException]
  override def addVertex[J >: K, U >: T](label: J): DirectedGraph[J, U] = {
    catching(classOf[ArithmeticException]).opt {
      label.asInstanceOf[K]
    }.foreach { lbl =>
      if (hasVertex(lbl))
        throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(lbl))
    }
    new DirectedGraph(new SimpleVertex[J, U](label) +: _vertices, _edges)
  }

  @throws[IllegalArgumentException]
  def addVertex[J >: K, U >: T](v: SimpleVertex[J, U]): DirectedGraph[J, U] = {
    catching(classOf[ArithmeticException]).opt {
      v.asInstanceOf[SimpleVertex[K, T]]
    }.flatMap { w =>
      if (hasVertex(w))
        None
      else
        Some(new DirectedGraph(v +: _vertices, _edges))
    }.getOrElse{
      throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(v))
    }
  }

  @throws[IllegalArgumentException]
  def addEdge[J >: K, U >: T](e: WeightedEdge[J, U]): DirectedGraph[J, U] = {
    catching(classOf[ArithmeticException]).opt {
      e.asInstanceOf[WeightedEdge[K, T]]
    }.flatMap { eK =>
      if (!hasVertex(eK.source) || !hasVertex(eK.source) || !hasEdge(eK))
        None
      else
        Some(new DirectedGraph(_vertices, e +: _edges))
    }.getOrElse {
      throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(e.toString))
    }
  }
}

object DirectedGraph {
  def apply[K, T](vertices: Seq[SimpleVertex[K, T]]) = new DirectedGraph[K, T](vertices, Nil)
  def apply[K, T](vertices: Seq[SimpleVertex[K, T]], edges: Seq[WeightedEdge[K, T]]) = new DirectedGraph[K, T](vertices, edges)
}

case object EmptyGraph extends DirectedGraph[Nothing, Nothing]() {
  override def equals(other: Any): Boolean = other match {
    case EmptyGraph =>
      true
    case _ =>
      false
  }

  override def hashCode(): Int = 0

}