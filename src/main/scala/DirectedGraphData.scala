package pharg

object DirectedGraphData {
  val empty = DirectedGraphData[Nothing, Nothing, Nothing](Set.empty, Set.empty, Map.empty, Map.empty)
}

case class DirectedGraphData[V, +VD, +ED](
  vertices: Set[V],
  edges: Set[Edge[V]],
  vertexData: Map[V, VD],
  edgeData: Map[Edge[V], ED]
) extends DirectedGraphLike[V] {

  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "V data can only be attached to existing vertices")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "E data can only be attached to existing edges")

  def +(v: V) = {
    assert(!(vertices contains v), s"V $v already exists in $vertices")
    copy(vertices = vertices + v)
  }

  def +(e: E) = {
    assert(!(edges contains e), s"E $e already exists in $edges")
    copy(edges = edges + e)
  }

  def -(v: V) = {
    assert(vertices contains v, s"V $v does not exist in $vertices")

    val (removedEdges, retainedEdges) = edges.partition(_ contains v)
    DirectedGraphData(
      vertices - v,
      retainedEdges,
      vertexData - v,
      edgeData -- removedEdges
    )
  }

  def -(e: E) = {
    assert(edges contains e, s"E $e does not exist in $edges")
    copy(edges = edges - e, edgeData = edgeData - e)
  }

  def inducedSubGraph(vp: (V) => Boolean): DirectedGraphData[V, VD, ED] = {
    val selectedEdges = inducedEdges(vp)
    val subGraph = DirectedGraphData(
      vertices.filter(vp),
      selectedEdges,
      vertexData filterKeys vp,
      edgeData filterKeys selectedEdges
    )
    assert(subGraph subGraphOf this)
    subGraph
  }

  def filterEdges(ep: (E) => Boolean): DirectedGraphData[V, VD, ED] = {
    copy(
      edges = edges filter ep,
      edgeData = edgeData filterKeys ep
    )
  }

  def --[ED1, VD1](subGraph: DirectedGraphData[V, ED1, VD1]) = {
    assert(subGraph subGraphOf this, s"Graph can only remove valid subgraph. $this -- $subGraph)")
    val removedVertices = subGraph.vertices
    val removedEdges = subGraph.edges ++ incidentEdges(subGraph.vertices)

    DirectedGraphData(
      vertices -- removedVertices,
      edges -- removedEdges,
      vertexData -- removedVertices,
      edgeData -- removedEdges
    )
  }

  def --(vs: Iterable[V]) = {
    //TODO: optimize
    vs.foldLeft(this)((graph, v) => graph - v)
  }

  def removeEdges(es: Iterable[E]) = {
    //TODO: optimize
    es.foldLeft(this)((graph, e) => graph - e)
  }

  def ++[VD1 >: VD, ED1 >: ED](that: DirectedGraphData[V, VD1, ED1]): DirectedGraphData[V, VD1, ED1] = {
    DirectedGraphData(
      this.vertices ++ that.vertices,
      this.edges ++ that.edges,
      this.vertexData ++ that.vertexData,
      this.edgeData ++ that.edgeData
    )
  }

  def mapVertices(m: V => V): DirectedGraphData[V, VD, ED] = {
    DirectedGraphData(
      vertices map m,
      edges map (_ map m),
      vertexData map { case (v, d) => m(v) -> d },
      edgeData map { case (e, d) => (e map m) -> d }
    )
  }

  def mapData[VD1, ED1](v: (VD) => VD1 = identity _, e: (ED) => ED1 = identity _): DirectedGraphData[V, VD1, ED1] = {
    copy(
      vertexData = vertexData mapValues v,
      edgeData = edgeData mapValues e
    )
  }

  def collectData[VD1, ED1](vp: PartialFunction[VD, VD1], ep: PartialFunction[ED, ED1]) = {
    // DirectedGraphData(
    //   vertices = vertices.filter(v => vp.isDefinedAt(vertexData(v))),
    //   edges = edges.filter(v => vp.isDefinedAt(edgeData(v))),
    //   vertexData = vertexData.collect{case (v,d) if vp.isDefinedAt(d) => (v,vp(d))},
    // )
    ???
  }

  override def toString = s"G(V(${vertices.toList.mkString(", ")}), " +
    s"E(${edges.toList.mkString(", ")})" +
    (if (vertexData.nonEmpty) s", {${vertexData.toList.map { case (v, d) => s"$v: $d" }.mkString(", ")}}" else "") +
    (if (edgeData.nonEmpty) s", {${edgeData.toList.map { case (Edge(in, out), d) => s"$in->$out: $d" }.mkString(", ")}}" else "") +
    ")"
}
