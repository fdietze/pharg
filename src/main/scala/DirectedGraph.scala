package pharg

import scala.collection.mutable

object DirectedGraph {
  val empty = DirectedGraph[Nothing](Set.empty, Set.empty)
}

case class DirectedGraph[V](
  vertices: Set[V],
  edges: Set[Edge[V]]
) extends DirectedGraphLike[V]

trait DirectedGraphLike[V] {
  type E = Edge[V]
  import pharg.{Edge => E}
  def vertices: Set[V]
  def edges: Set[E]

  assert(edges.flatMap(e => List(e.in, e.out)) subsetOf vertices, "Edges can only connect existing vertices")

  def isolatedVertices = vertices.filter(degree(_) == 0)
  def inDegree(v: V) = predecessors(v).size
  def outDegree(v: V) = successors(v).size
  def degree(v: V) = inDegree(v) + outDegree(v)
  def numElements = vertices.size + edges.size

  // lazy caching datastructure for successors, predecessors, incomingEdges, outgoingEdges
  private def MapVVempty = Map.empty[V, Set[V]].withDefault((v: V) => { assert(vertices contains v); Set.empty[V] })
  private def MapVEempty = Map.empty[V, Set[E]].withDefault((v: V) => { assert(vertices contains v); Set.empty[E] })

  lazy val successors: Map[V, Set[V]] = edges.foldLeft(MapVVempty) { case (suc, E(in, out)) => suc + (in -> (suc(in) + out)) }
  lazy val predecessors: Map[V, Set[V]] = edges.foldLeft(MapVVempty) { case (pre, E(in, out)) => pre + (out -> (pre(out) + in)) }
  lazy val incomingEdges: Map[V, Set[E]] = edges.foldLeft(MapVEempty) { case (incoming, edge @ E(_, out)) => incoming + (out -> (incoming(out) + edge)) }
  lazy val outgoingEdges: Map[V, Set[E]] = edges.foldLeft(MapVEempty) { case (outgoing, edge @ E(in, _)) => outgoing + (in -> (outgoing(in) + edge)) }
  // def successors(v: V) = { assert(vertices contains v); edges.collect { case E(`v`, out) => out } }
  // def predecessors(v: V) = { assert(vertices contains v); edges.collect { case E(in, `v`) => in } }
  // def outgoingEdges(v: V) = { assert(vertices contains v); edges.filter(_.in == v) }
  // def incomingEdges(v: V) = { assert(vertices contains v); edges.filter(_.out == v) }

  def neighbours(v: V): Set[V] = {
    assert(vertices contains v)
    edges.collect {
      case E(`v`, out) => out
      case E(in, `v`) => in
    }
  }

  // open neighbourhood
  def neighbours(vp: (V) => Boolean): Set[V] = {
    vertices.filter(vp).flatMap(neighbours).filterNot(vp)
  }

  def incidentEdges(v: V): Set[E] = {
    assert(vertices contains v)
    edges.filter(_ contains v)
  }

  def incidentEdges(vp: (V) => Boolean): Set[E] = {
    edges.filter(e => vp(e.in) || vp(e.out))
  }

  def inducedEdges(vp: (V) => Boolean): Set[E] = {
    edges.filter(e => vp(e.in) && vp(e.out))
  }

  def topologicalSort: List[V] = {
    assert(!hasCycle)

    var sorted: List[V] = Nil
    val unmarked = mutable.HashSet.empty[V] ++ vertices
    val tempMarked = mutable.HashSet.empty[V]

    while (unmarked.nonEmpty) visit(unmarked.head)

    def visit(n: V) {
      if (unmarked(n)) {
        tempMarked += n
        for (m <- successors(n)) visit(m)
        unmarked -= n
        tempMarked -= n
        sorted ::= n
      }
    }

    sorted
  }

  def depthFirstSearch(start: V, continue: V => Iterable[V] = successors) = new Iterator[V] {
    assert(vertices contains start)

    val stack = mutable.Stack(start)
    val onStack = mutable.Set[V]()
    val seen = mutable.Set[V]()

    override def hasNext: Boolean = stack.nonEmpty
    override def next: V = {
      val current = stack.pop
      onStack -= current
      seen += current

      for (candidate <- continue(current)) {
        if (!seen(candidate) && !onStack(candidate)) {
          stack push candidate
          onStack += candidate
        }
      }

      current
    }
  }

  def isEmpty = vertices.isEmpty

  def subGraphOf(superGraph: DirectedGraphLike[V]) = {
    (this.vertices subsetOf superGraph.vertices) &&
      (this.edges subsetOf superGraph.edges)
  }

  //TODO: rename? undirected case with n*(n-1)/2?
  def isComplete = {
    val n = vertices.size
    edges.size == n * (n - 1)
  }

  def connectedComponents: Set[Set[V]] = connectedComponents(neighbours)
  def connectedComponents(neighbourSelector: V => Iterable[V]): Set[Set[V]] = {
    val toVisit = mutable.HashSet.empty ++ vertices
    val components = mutable.HashSet.empty[Set[V]]
    while (toVisit.nonEmpty) {
      val start = toVisit.head
      val component = depthFirstSearch(start, neighbourSelector).toSet
      components += component
      toVisit --= component
    }
    components.toSet
  }

  //TODO: optimization: isComplete || depthFirst...
  def isConnected = isEmpty || depthFirstSearch(vertices.head, neighbours).size == vertices.size

  def hasCycle: Boolean = {
    //TODO: optimization: use linear time algorithm
    val next = mutable.HashSet.empty ++ vertices

    while (next.nonEmpty) {
      if (cycleAt(next.head)) return true
    }

    def cycleAt(v: V, visited: Set[V] = Set.empty): Boolean = {
      if (visited contains v) return true // found cycle
      if (!(next contains v)) return false // we already checked from here, there is definitely no cycle

      next -= v
      successors(v).exists(cycleAt(_, visited + v))
    }

    false
  }

  def isIsomorphicTo(that: DirectedGraphLike[V]): Boolean = {
    if (this.vertices.size != that.vertices.size) return false
    if (this.edges.size != that.edges.size) return false
    if (this.vertices.toList.map(this.degree).sorted != that.vertices.toList.map(that.degree).sorted) return false

    if (this.vertices.size > 20) println(s"Isomorphism testing on Graph with ${this.vertices.size} vertices...")

    val thisLabels = this.vertices
    val thatLabels = that.vertices

    def prune(vA: V, vB: V, perm: Map[V, V]) = {
      // val (vA, vB) = (V(candA), V(candB))
      this.inDegree(vA) != that.inDegree(vB) ||
        this.outDegree(vA) != that.outDegree(vB) ||
        perm.toList.exists { case (a, b) => this.edges.contains(E(a, vA)) != that.edges.contains(E(b, vB)) } ||
        perm.toList.exists { case (a, b) => this.edges.contains(E(vA, a)) != that.edges.contains(E(vB, b)) }
      // !(this.inducedSubGraph((thisLabels -- perm.keySet - candA).map(V(_))) isIsomorphicTo that.inducedSubGraph((thatLabels -- perm.values - candB).map(V(_))))
    }

    def recurse(perm: Map[V, V]): Boolean = {
      if (perm.size == this.vertices.size) {
        vertices.map(perm) == that.vertices &&
          edges.map(_ map perm) == that.edges
      } else {
        val thisCandidates = thisLabels -- perm.keySet
        val thatCandidates = thatLabels -- perm.values
        val thisCandidate = thisCandidates.head
        thatCandidates.filterNot(prune(thisCandidate, _, perm)).exists { thatCandidate =>
          recurse(perm + (thisCandidate -> thatCandidate))
        }
      }
    }

    recurse(Map.empty)
  }
}
