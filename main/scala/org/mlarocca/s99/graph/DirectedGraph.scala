package org.mlarocca.s99.graph

import org.mlarocca.s99.graph

import scala.collection.mutable
import scala.util.control.Exception._

class DirectedGraph[K <% Ordered[K], T](
    _vertices: Seq[SimpleVertex[K, T]] = Nil,
    _edges: Seq[WeightedEdge[K, T]] = Nil) extends Graph[K, T](_vertices, _edges) {
  import graph.DirectedGraph._

  override def getVertex(key: K): SimpleVertex[K, T] = super.getVertex(key).asInstanceOf[SimpleVertex[K,T]]
  override def getVertexOption(key: K): Option[SimpleVertex[K, T]] = super.getVertexOption(key).asInstanceOf[Option[SimpleVertex[K,T]]]


  @throws[IllegalArgumentException]
  override def addVertex[J >: K <% Ordered[J], U >: T](label: J): DirectedGraph[J, U] = {
    catching(classOf[ArithmeticException]).opt {
      label.asInstanceOf[K]
    }.foreach { lbl =>
      if (hasVertex(lbl))
        throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(lbl))
    }
    new DirectedGraph(new SimpleVertex[J, U](label) +: _vertices, _edges)
  }

  @throws[IllegalArgumentException]
  def addVertex[J >: K <% Ordered[J], U >: T](v: SimpleVertex[J, U]): DirectedGraph[J, U] = {
    catching(classOf[ArithmeticException]).opt {
      v.asInstanceOf[SimpleVertex[K, T]]
    }.flatMap { w =>
      if (hasVertex(w))
        None
      else {
        Some(new DirectedGraph(v +: _vertices, _edges))
      }
    }.getOrElse{
      throw new IllegalArgumentException(Graph.DuplicateVertexExceptionMessage.format(v))
    }
  }

  @throws[IllegalArgumentException]
  def addEdge[J >: K <% Ordered[J], U >: T](e: WeightedEdge[J, U]): DirectedGraph[J, U] = {
    catching(classOf[ArithmeticException]).opt {
      e.asInstanceOf[WeightedEdge[K, T]]
    }.flatMap { eK =>
      if (!hasVertex(eK.source) || !hasVertex(eK.destination) || hasEdge(eK)) {
        None
      } else {
        val source = getVertex(eK.source).asInstanceOf[SimpleVertex[K, T]]
        val newSource = new SimpleVertex[J, U](source.key, eK +: source.adj)
        val newVerticesSeq = newSource +: _vertices.filter(_ != source)
        Some(new DirectedGraph(newVerticesSeq, e +: _edges))
      }
    }.getOrElse {
      throw new IllegalArgumentException(Graph.InvalidEdgeExceptionMessage.format(e.toString()))
    }
  }

  override def toString(): String = {
    val edgesStr = edges.filter { e =>
      val Edge(u, v) = e
      !this.hasEdge(v, u) || u.compare(v) <= 0
    }.map { e =>
      val Edge(u, v) = e
      if (this.hasEdge(v, u))
        s"$u - $v"
      else
        s"$u > $v"
    }
    s"[${edgesStr.toSeq.sorted mkString EdgesListSeparator}]"
  }

  /**
   * Single source all destinations bfs search.
   *
   * @return
   */
  @throws[IllegalArgumentException]
  override def bfs(
      source: K): SearchResult[K] =  {
    singleSourceSearchTemplate(_ => 1)(source)
  }

  /**
   * Source to goal bfs search.
   *
   * @return
   */
  @throws[IllegalArgumentException]
  override def bfs(
      source: K,
      goal: K): SearchResult[K] =  {
    aStarTemplate(_ => 1, (_, _) => 0)(source, goal)
  }

  /**
   * Single source all destinations Dijkstra search
   * @return
   */
  @throws[IllegalArgumentException]
  override def dijkstra(
      source: K): SearchResult[K] =  {
    singleSourceSearchTemplate(_.weight)(source)
  }

  /**
   * Source to goal Dijkstra search
   * @return
   */
  @throws[IllegalArgumentException]
  override def dijkstra(
      source: K,
      goal: K): SearchResult[K] =  {
    aStarTemplate(_.weight, (_, _) => 0)(source, goal)
  }

  @throws[IllegalArgumentException]
  override def dfs(source: K): SearchResult[K]  = {
    val notVisited = mutable.Set.empty[K] ++ vertices.map(_.key)
    val predecessors = mutable.Map[K, K]()
    val distances = mutable.Map[K, Double]().withDefaultValue(Double.MaxValue)

    if (!hasVertex(source)) {
      throw new IllegalArgumentException(IllegalVertexExceptionMessage.format(source))
    }

    def doDfs(v: K, pred: K, entryTime: Int): Int = {
      notVisited.remove(v)
      predecessors.put(v, pred)
      val exitTime: Int = 1 + getVertex(v).neighbors.foldLeft(entryTime) { (dist: Int, u: K) =>
        if (notVisited.contains(u))
          doDfs(u, v, dist + 1)
        else
          dist
      }
      distances.put(v, exitTime)
      exitTime
    }

    doDfs(source, source, 0)

    while (notVisited.nonEmpty) {
      val u = notVisited.head
      doDfs(u, u, distances.values.max.toInt + 1)
    }

    //Converts to immutable maps
    SearchResult(distances.toMap, predecessors.toMap)
  }

  @throws[IllegalArgumentException]
  private[graph] def singleSourceSearchTemplate(
      distanceFunction: (WeightedEdge[K,T]) => Double)(
      source: K): SearchResult[K] = {
    def init(): (mutable.PriorityQueue[VertexWithDistance], mutable.Map[K, K], mutable.Map[K, Double]) = {
      val predecessors = mutable.Map[K, K]()
      val queue = new mutable.PriorityQueue[VertexWithDistance]()(VertexWithDistanceOrdering)
      val distances = mutable.Map[K, Double]().withDefaultValue(Double.MaxValue)
      (queue, predecessors, distances)
    }

    def addVertexToQueueGeneric(
        queue: mutable.PriorityQueue[VertexWithDistance],
        predecessors: mutable.Map[K, K],
        distances: mutable.Map[K, Double])
        (v: K, predecessor: K, distance: Double) {
      queue.enqueue(VertexWithDistance(getVertex(v), distance))
      predecessors.put(v, predecessor)
      distances.put(v, distance)
    }

    if (!hasVertex(source)) {
      throw new IllegalArgumentException(IllegalVertexExceptionMessage.format(source))
    }

    val (queue, predecessors, distances) = init()
    val addVertexToQueue = addVertexToQueueGeneric(queue, predecessors, distances) _
    lazy val n = vertices.size

    addVertexToQueue(source, source, 0)

    do {
      val current = queue.dequeue()
      val v = getVertex(current.vertex.asInstanceOf[SimpleVertex[K,T]].key)
      current.vertex.adj.foreach { e =>
        val u = e.destination.asInstanceOf[K]
        val newDist = distanceFunction(e.asInstanceOf[WeightedEdge[K,T]]) + current.distance
        if (distances(u) > newDist) {
          addVertexToQueue(u, current.vertex.asInstanceOf[SimpleVertex[K, T]].key,  newDist)
        }
      }
    } while (queue.nonEmpty && predecessors.size < n)
    //Converts to immutable maps
    SearchResult(distances.toMap, predecessors.toMap)
  }

  @throws[IllegalArgumentException]
  private[graph] def aStarTemplate(
      distance: (WeightedEdge[K,T]) => Double,
      heuristic: (K, K) => Double)(
      source: K,
      goal: K): SearchResult[K] = {

    def init(): (mutable.PriorityQueue[VertexWithDistance], mutable.Map[K, K], mutable.Map[K, Double]) = {
      val predecessors = mutable.Map[K, K]()
      val queue = new mutable.PriorityQueue[VertexWithDistance]()(VertexWithDistanceOrdering)
      val distances = mutable.Map[K, Double]().withDefaultValue(Double.MaxValue)
      (queue, predecessors, distances)
    }

    def addVertexToQueueGeneric(
        queue: mutable.PriorityQueue[VertexWithDistance],
        predecessors: mutable.Map[K, K],
        distances: mutable.Map[K, Double])
        (v: K, predecessor: K, distance: Double) {
      queue.enqueue(VertexWithDistance(getVertex(v), distance))
      predecessors.put(v, predecessor)
      distances.put(v, distance)
    }

    if (!hasVertex(source)) {
      throw new IllegalArgumentException(IllegalVertexExceptionMessage.format(source))
    }

    if (!hasVertex(goal)) {
      throw new IllegalArgumentException(IllegalVertexExceptionMessage.format(goal))
    }

    val curriedHeuristic = heuristic.curried(goal)

    val (queue, predecessors, distances) = init()
    val addVertexToQueue = addVertexToQueueGeneric(queue, predecessors, distances) _

    addVertexToQueue(source, source, 0)

    do {
      val current = queue.dequeue()
      if (current.vertex == goal) {
        //We have found the goal vertex, so we can break out of the loop
        queue.clear()
      } else {
        current.vertex.adj.foreach { e =>
          val u = e.destination.asInstanceOf[K]
          val newDist = current.distance + distance(e.asInstanceOf[WeightedEdge[K,T]])
          if (distances(u) > newDist ) {
            addVertexToQueue(u, current.vertex.asInstanceOf[SimpleVertex[K, T]].key, newDist + curriedHeuristic(u))
          }
        }
      }
    } while (queue.nonEmpty)

    //Converts to immutable maps
    val predecessorsIM = predecessors.toMap
    SearchResult(distances.toMap, predecessorsIM, reconstructPath[K](predecessorsIM)(source, goal))
  }

}

object DirectedGraph {
  def apply[K <% Ordered[K], T](vertices: Seq[SimpleVertex[K, T]]) = new DirectedGraph[K, T](vertices, Nil)
  def apply[K <% Ordered[K], T](vertices: Seq[SimpleVertex[K, T]], edges: Seq[WeightedEdge[K, T]]) = new DirectedGraph[K, T](vertices, edges)

  @throws[IllegalArgumentException]
  implicit def fromString(s: String): DirectedGraph[String, String] = s match {
    case ValidGraphString(edgesStr) =>
      val edgesDec = edgesStr.split(EdgesListSeparator)
        .map(_.trim)
        .filter(!_.isEmpty)
        .map {
          case ValidWeightedEdgeString(src, verse, dst, weight) =>
            EdgeDecomposition(src, verse, dst, Some(weight.toDouble))
          case ValidEdgeString(src, verse, dst) =>
            EdgeDecomposition(src, verse, dst)
          case _ =>
            throw new IllegalArgumentException(UnParsableStringExceptionMessage.format(s))
        }

      val newVertices = edgesDec.flatMap {
        case EdgeDecomposition(src, _, dst, _) =>
          Set(src, dst)
      }.toSet[String]

      val newEdges = edgesDec.flatMap {
        case EdgeDecomposition(src, verse, dst, Some(weight)) => verse match {
          case ">" =>
            Seq(WeightedEdge[String](src, dst, weight = weight))
          case "-" if src != dst =>
            Seq(WeightedEdge[String](src, dst, weight = weight), WeightedEdge[String](dst, src, weight = weight))
          case "-" =>
            Seq(WeightedEdge[String](src, dst, weight = weight))
        }
        case EdgeDecomposition(src, verse, dst, None) => verse match {
          case ">" =>
            Seq(WeightedEdge[String](src, dst))
          case "-" if src != dst =>
            Seq(WeightedEdge[String](src, dst), WeightedEdge[String](dst, src))
          case "-" =>
            Seq(WeightedEdge[String](src, dst))
        }
      }
      val tmpG = newVertices.foldLeft[DirectedGraph[String, String]](EmptyGraph[String, String]){ (G, v) =>
        G.addVertex(v)
      }

      newEdges.foldLeft(tmpG){ (G, e) =>
        G.addEdge(e)
      }
    case _ =>
      throw new IllegalArgumentException(UnParsableStringExceptionMessage.format(s))
  }

  private[graph] def reconstructPath[K](predecessors: Map[K, K])(source: K, goal: K): Option[Seq[K]] = {
    val n = predecessors.size
    def recursivePath(current: Option[K], counter: Int): Option[Seq[K]] = current match {
      case None => None
      case Some(`source`) => Some(Seq(source))
      case _ if counter >= n => None //the path would be longer than the number of vertices
      case Some(v) =>
        recursivePath(predecessors.get(v), counter + 1).map { rest =>
          v +: rest
        }
    }

    recursivePath(Some(goal), 0).map {
      _.reverse
    }
  }

  def EmptyGraph[K <% Ordered[K], T] = new DirectedGraph[K, T]()

  case class SearchResult[K <% Ordered[K]](
      distances: Map[K, Double],
      predecessors: Map[K, K],
      path: Option[Seq[K]] = None)

  private[graph] val EdgesListSeparator = ", "
  private[graph] lazy val ValidGraphString = """\[((?:(?:[^\[\]]+)(?:,\s[^\[\],\s]+)*)?)\]""".r
  //Note: Greedy quantifiers in the groups below
  private[graph] lazy val ValidEdgeString = """([^\[\]\(\)\->,]+?)\s(\-|>)\s([^\[\]\(\)\->,]+?)""".r
  private[graph] lazy val ValidWeightedEdgeString = """([^\[\]\(\)\->,]+?)\s(\-|>)\s([^\[\]\(\)\->,]+?)\s+\((\d+(?:\.\d+)*)\)""".r

  private[graph] val UnParsableStringExceptionMessage = "String %s is not a valid Graph"
  private[graph] val IllegalVertexExceptionMessage = "Vertex %s is not part of this Graph"

  private case class EdgeDecomposition(src: String, verse: String, dst: String, weight: Option[Double] = None)
  private case class VertexWithDistance(vertex: SimpleVertex[_, _], distance: Double)

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

