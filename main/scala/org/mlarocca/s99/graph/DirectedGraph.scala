package org.mlarocca.s99.graph

import org.mlarocca.s99.graph

import scala.collection.mutable
import scala.util.control.Exception._

class DirectedGraph[K, T](
    _vertices: Seq[SimpleVertex[K, T]] = Nil,
    _edges: Seq[WeightedEdge[K, T]] = Nil) extends Graph[K, T](_vertices, _edges) {
  import graph.DirectedGraph._

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

  override def toString(): String = {
    val edgesStr = edges.filter { e =>
      val Edge(u, v) = e
      !this.hasEdge(v, u) || u.compare(v) <= 0
    }.map { e =>
      val Edge(u, v) = e
      if (this.hasEdge(v, u))
        s"${u.key} - ${v.key}"
      else
        s"${u.key} > ${v.key}"
    }
    s"[${edgesStr.toSeq.sorted mkString EdgesListSeparator}]"
  }

  @throws[IllegalArgumentException]
  def bfs(source: SimpleVertex[K, T]): SearchResult[K, T] = {
    def init(): (mutable.PriorityQueue[VertexWithDistance], mutable.Map[SimpleVertex[K, T], SimpleVertex[K,T]], mutable.Map[SimpleVertex[K, T], Double]) = {
      val predecessors = mutable.Map[SimpleVertex[K, T], SimpleVertex[K,T]]()
      val queue = new mutable.PriorityQueue[VertexWithDistance]()(VertexWithDistanceOrdering)
      val distances = mutable.Map[SimpleVertex[K, T], Double]().withDefaultValue(Double.MaxValue)
      (queue, predecessors, distances)
    }

    def addVertexToQueueGeneric(
        queue: mutable.PriorityQueue[VertexWithDistance],
        predecessors: mutable.Map[SimpleVertex[K, T], SimpleVertex[K,T]],
        distances: mutable.Map[SimpleVertex[K, T], Double])
      (v: SimpleVertex[K, T], predecessor: SimpleVertex[K,T], distance: Double) {
      queue.enqueue(VertexWithDistance(v, distance))
      predecessors.+(v -> predecessor)
      distances.+(v -> distance)
    }

    if (!hasVertex(source)) {
      throw new IllegalArgumentException(IllegalVertexExceptionMessage.format(source))
    }

    val (queue, predecessors, distances) = init()
    val addVertexToQueue = addVertexToQueueGeneric(queue, predecessors, distances)
    lazy val n = vertices.size

    addVertexToQueue(source, null, 0)

    do {
      val current = queue.dequeue()
      current.vertex.adj.foreach { e =>
        val (u, weight) = (e.destination.asInstanceOf[SimpleVertex[K, T]], e.weight)
        if (distances(u) > weight + current.distance) {
          addVertexToQueue(u, current.vertex.asInstanceOf[SimpleVertex[K, T]],  weight + current.distance)
        }
      }

    } while (!queue.isEmpty && predecessors.size < n)

    //Converts to immutable maps
    SearchResult(distances.toMap, predecessors.toMap)
  }

  @throws[IllegalArgumentException]
  def dfs(source: SimpleVertex[K, T]): SearchResult[K, T]  = {
    val predecessors = mutable.Map[SimpleVertex[K, T], SimpleVertex[K,T]]()
    val distances = mutable.Map[SimpleVertex[K, T], Double]().withDefaultValue(Double.MaxValue)

    def doDfs(v: SimpleVertex[K, T], discoveryTime: Int): Int = {
      val exitTime: Int = v.neighbors.foldLeft(discoveryTime) { (dist: Int, u: Vertex[K, T]) =>
        doDfs(u.asInstanceOf[SimpleVertex[K, T]], dist + 1)
      }
      distances.+(v -> exitTime)
      exitTime
    }

    doDfs(source, 0)
    //Converts to immutable maps
    SearchResult(distances.toMap, predecessors.toMap)
  }
}

object DirectedGraph {
  def apply[K, T](vertices: Seq[SimpleVertex[K, T]]) = new DirectedGraph[K, T](vertices, Nil)
  def apply[K, T](vertices: Seq[SimpleVertex[K, T]], edges: Seq[WeightedEdge[K, T]]) = new DirectedGraph[K, T](vertices, edges)

  @throws[IllegalArgumentException]
  implicit def fromString(s: String): DirectedGraph[String, String] = s match {
    case ValidGraphString(edgesStr) =>
      val edgesDec = edgesStr.split(EdgesListSeparator)
        .map(_.trim)
        .filter(!_.isEmpty)
        .map {
          case ValidEdgeString(src, verse, dst) =>
            EdgeDecomposition(src, verse, dst)
          case _ =>
            throw new IllegalArgumentException(UnParsableStringExceptionMessage.format(s))
        }

      val newVertices = edgesDec.flatMap {
        case EdgeDecomposition(src, verse, dst) =>
          Set(src, dst)
      }.toSeq.map(SimpleVertex.apply)

      val newEdges = edgesDec.flatMap {
        case EdgeDecomposition(src, verse, dst) => verse match {
          case ">" =>
            Seq(WeightedEdge[String](src, dst))
          case "-" =>
            Seq(WeightedEdge[String](src, dst), WeightedEdge[String](dst, src))
        }
      }

      new DirectedGraph[String, String](newVertices, newEdges)
    case _ =>
      throw new IllegalArgumentException(UnParsableStringExceptionMessage.format(s))
  }

  private[graph] val EdgesListSeparator = ", "
  private[graph] lazy val ValidGraphString = """\[((?:(?:[^\[\]]+)(?:,\s[^\[\],\s]+)*)?)\]""".r
  private[graph] lazy val ValidEdgeString = """([^\[\]\->,]+)\s(\-|>)\s([^\[\]\->,]+)""".r
  private[graph] val UnParsableStringExceptionMessage = "String %s is not a valid Graph"
  private[graph] val IllegalVertexExceptionMessage = "Vertex %s is not part of this Graph"

  private case class EdgeDecomposition(src: String, verse: String, dst: String)
  private case class VertexWithDistance(vertex: SimpleVertex[_, _], distance: Double)
  private case class SearchResult[K, T](distances: Map[SimpleVertex[K, T], Double], predecessors: Map[SimpleVertex[K, T], SimpleVertex[K, T]])

  private final val VertexWithDistanceOrdering = new Ordering[VertexWithDistance] {
    override def compare(x: VertexWithDistance, y: VertexWithDistance): Int = {
      val (xDist, yDist) = (x.distance, y.distance)
      if (Math.abs(xDist - yDist) < 10e-20) {
        0
      } else if (xDist < yDist) {
        -1
      } else {
        1
      }
    }
  }
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