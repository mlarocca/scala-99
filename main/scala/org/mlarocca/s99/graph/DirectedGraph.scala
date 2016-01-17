package org.mlarocca.s99.graph

import scala.util.control.Exception._

class DirectedGraph[K](
    _vertices: Seq[SimpleVertex[K]] = Nil,
    _edges: Seq[WeightedEdge[K]] = Nil) extends Graph[K](
  _vertices, _edges) {

  @throws[IllegalArgumentException]
  def addVertex[T >: K](label: T): Graph[T] = {
    catching(classOf[ArithmeticException]).opt {
      label.asInstanceOf[K]
    }.foreach { lbl =>
      if (hasVertex(lbl))
        throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(lbl))
    }
    new DirectedGraph(new SimpleVertex[T](label) +: _vertices, _edges)
  }

  def addVertex[T >: K](v: SimpleVertex[T]): Graph[T] = {
    catching(classOf[ArithmeticException]).opt {
      v.asInstanceOf[SimpleVertex[K]]
    }.foreach { w =>
      if (hasVertex(w))
        throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(v.key))
    }
    new DirectedGraph(v +: _vertices, _edges)
  }
}

object DirectedGraph {
  def apply[K](vertices: Seq[SimpleVertex[K]]) = new DirectedGraph[K](vertices, Nil)
  def apply[K](vertices: Seq[SimpleVertex[K]], edges: Seq[WeightedEdge[K]]) = new DirectedGraph[K](vertices, edges)
}


case object EmptyGraph extends DirectedGraph[Nothing]()