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
   * Is the graph connected?
   * To ascertain it, we run a dfs starting from the largest vertex in the topological order:
   * the graph is connected iff it is possible to reach every other vertex from it.
   * An empty graph is considered not connected.
   *
   * @return
   */
  def isConnected(): Boolean = {
    //empty graph will be considered not connected
    topologicalSort().headOption.exists { v =>
      val DfsSearchResult(_, _, _, connectedComponents) = dfs(Some(v.key))
      connectedComponents == 1
    }
  }

  def isAcyclic(): Boolean = {
    val DfsSearchResult(_, _, acyclic, _) = dfs()
    acyclic
  }

  def isTree(): Boolean = isConnected() && isAcyclic()

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

  /**
   * Computes the DFS for the graph, either starting from a random vertex or from the one passed as optinal argument.
   * If the graph is not connected, performs several DFS searches until all the vertices are visited.
   *
   * @param start Optionally a starting point can be provided.
   * @throws NoSuchElementException If start is passed and it's not a valid vertex.
   * @return It returns a complex result with:
   *         1. The exit times for each and all vertices (used for topological sort).
   *         2. The predecessors for each vertex. Start vertices for each DFSCycle are predecessors of themselves.
   *         3. A boolean showing if the graph is acyclic.
   *         4. The number of connected components in the graph.
   */
  @throws[NoSuchElementException]
  override def dfs(start: Option[K] = None): DfsSearchResult[K]  = {
    val notVisited = mutable.Set.empty[K] ++ vertices.map(_.key)
    val predecessors = mutable.Map[K, K]()
    val exitTimes = mutable.Map[K, Double]().withDefaultValue(Double.MaxValue)

    def doDfs(v: K, pred: K, entryTime: Int, notVisited: mutable.Set[K]): (Int, Boolean) = {
      notVisited.remove(v)
      predecessors.put(v, pred)
      val (exitTime, isAcyclic) = getVertex(v).neighbors.foldLeft((entryTime, true)) { (accum: (Int, Boolean), u: K) =>
        val (time, acyclic) = accum
        if (notVisited.contains(u)) {
          val (eTimeR, acyclicR) = doDfs(u, v, time, notVisited)
          (eTimeR, acyclicR && acyclic)
        } else {
          //It's acyclic iff the vertex is "Black", i.e. it was visited in a prev call to dfsCycle and form u it was
          //not possible to reach v. If u is "Gray", then we arrived to v from u, and there is a cycle.
          (time, exitTimes.contains(u))
        }
      }
      exitTimes.put(v, 1 + exitTime)
      (1 + exitTime, isAcyclic)
    }

    def dfsCycle(notVisited: mutable.Set[K], time: Int, next: Option[K] = None): (Int, Boolean) = {
      if (notVisited.isEmpty) {
        (0, true)
      } else {
        val u = next.getOrElse(notVisited.head)
        val (exitTime, isAcyclicComponent) = doDfs(u, u, time, notVisited)
        val (components, isAcyclic) = dfsCycle(notVisited, exitTime)
        (1 + components, isAcyclic && isAcyclicComponent)
      }
    }

    val (components, isAcyclic) = dfsCycle(notVisited, 0, start)

    //Converts to immutable maps
    DfsSearchResult(exitTimes.toMap, predecessors.toMap, isAcyclic, components)
  }

  /**
   * Topological order of the vertices.
   * The order is well defined only for a subset of all graphs.
   * For most graphs, the order may vary on execution.
   * For acyclic graphs, if v -> u then it will be u < v, but if there is no edge between u and v, they can
   * appear in any order.
   *
   * @return
   */
  def topologicalSort(): Seq[SimpleVertex[K, T]] = {
    val DfsSearchResult(exitTimes, _ , _, _) = dfs()
    _vertices.sortBy(v => exitTimes(v.key)).reverse
  }

  /**
   * Return all the acyclic paths between two vertices.
   *
   * @param start The starting point for the path.
   * @param end The destination in the path.
   * @throws NoSuchElementException if the vertices does not belong to the graph
   * @return
   */
  @throws[NoSuchElementException]
  def allAcyclicPaths(start: K, end: K): Set[Seq[SimpleVertex[K, T]]] = {
    val endVertex = getVertex(end)

    def dfsPath(current: K, path: Seq[SimpleVertex[K, T]], visited: Set[K]): Set[Seq[SimpleVertex[K, T]]] = {
      getVertex(current).neighbors[K].flatMap {
        _ match {
          case `end` =>
            Set(endVertex +: path)
          case v if !visited.contains(v) =>
            dfsPath(v, getVertex(v) +: path, visited.+(v))
          case _ =>
            Nil
        }
      }
    }
    dfsPath(start, Seq(getVertex(start)), Set(start)).map(_.reverse)
  }

  def allMST():Set[Set[WeightedEdge[K, T]]] = {
    val n = _vertices.size
    def buildMST(
        maybeTree: Seq[WeightedEdge[K, T]],
        remainingEdges: Seq[WeightedEdge[K, T]]): Set[Set[WeightedEdge[K, T]]] = {
      lazy val G = new DirectedGraph[K, T](_vertices, maybeTree)
      lazy val acyclic = G.isAcyclic()
      lazy val connected = G.isConnected()
      (acyclic, connected, remainingEdges) match {
        case (false, _, _) =>
          //A cyclic graph can't be a tree.
          Set.empty
        case (true, false, _) if (maybeTree.size >= n) =>
          //Too many edges to be a tree, ever
          Set.empty
        case (true, true, _) if (maybeTree.size > n) =>
          //Too many edges to be a tree, ever
          Set.empty
        case (true, true, _) if (maybeTree.size == n) =>
          //If the graph is acyclic and connected with n edges, then it's a tree. Adding more edges would make it cyclic.
          Set(maybeTree.toSet)
        case (true, true, e :: rest) =>
          buildMST(e +: maybeTree, rest) ++ buildMST(maybeTree, rest)
      }
    }
    buildMST(Nil, _edges)
  }

  /**
   * Return all the simple cyclic paths starting and ending on one vertex.
   *
   * @param start The starting point for the path.
   * @throws NoSuchElementException if the vertices does not belong to the graph
   * @return
   */
  @throws[NoSuchElementException]
  def allSimpleCycles(start: K): Set[Seq[SimpleVertex[K, T]]] = {
    allAcyclicPaths(start, start)
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
            EdgeStringDecomposition(src, verse, dst, Some(weight.toDouble))
          case ValidEdgeString(src, verse, dst) =>
            EdgeStringDecomposition(src, verse, dst)
          case ValidVertexString(v) =>
            EdgeStringDecomposition(v, null, null)
          case _ =>
            throw new IllegalArgumentException(UnParsableStringExceptionMessage.format(s))
        }

      val newVertices = edgesDec.flatMap {
        case EdgeStringDecomposition(v, null, null, _) =>
          Set(v)
        case EdgeStringDecomposition(src, _, dst, _) =>
          Set(src, dst)
      }.toSet[String]

      val newEdges = edgesDec.flatMap {
        case EdgeStringDecomposition(v, null, null, _) =>
          Nil
        case EdgeStringDecomposition(src, verse, dst, Some(weight)) => verse match {
          case ">" =>
            Seq(WeightedEdge[String](src, dst, weight = weight))
          case "-" if src != dst =>
            Seq(WeightedEdge[String](src, dst, weight = weight), WeightedEdge[String](dst, src, weight = weight))
          case "-" =>
            Seq(WeightedEdge[String](src, dst, weight = weight))
        }
        case EdgeStringDecomposition(src, verse, dst, None) => verse match {
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

  case class DfsSearchResult[K <% Ordered[K]](
      exitTimes: Map[K, Double],
      predecessors: Map[K, K],
      isAcyclic: Boolean,
      connectedComponents: Int)


  private[graph] val EdgesListSeparator = ", "
  private[graph] lazy val ValidGraphString = """\[((?:(?:[^\[\]]+)(?:,\s[^\[\],\s]+)*)?)\]""".r
  //Note: Greedy quantifiers in the groups below
  private[graph] lazy val ValidEdgeString = """([^\[\]\(\)\->,]+?)\s(\-|>)\s([^\[\]\(\)\->,]+?)""".r
  private[graph] lazy val ValidWeightedEdgeString = """([^\[\]\(\)\->,]+?)\s(\-|>)\s([^\[\]\(\)\->,]+?)\s+\((\d+(?:\.\d+)*)\)""".r
  private[graph] lazy val ValidVertexString = """([^\[\]\(\)\->,]+?)""".r

  private[graph] val UnParsableStringExceptionMessage = "String %s is not a valid Graph"
  private[graph] val IllegalVertexExceptionMessage = "Vertex %s is not part of this Graph"

  private case class EdgeStringDecomposition(src: String, verse: String, dst: String, weight: Option[Double] = None)
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

