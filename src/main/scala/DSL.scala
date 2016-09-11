package pharg

object DSL {
  //TODO: graph dsl: LinkedHashSet for predictable vertex/edge traversal in tests
  //TODO: implicits for accessors: V,E

  // Graph construction
  def V(range: Range) = range.toSet
  def V[Vertex](vs: Vertex*) = Set(vs: _*)
  def e[Vertex](edge: (Vertex, Vertex)) = edge match { case (in, out) => Edge(in, out) }
  def E[Vertex](es: (Vertex, Vertex)*) = es.map { case (in, out) => Edge(in, out) }.toSet
  // def G[Vertex](
  //   V: Set[Vertex] = Set.empty[Vertex],
  //   E: Set[Edge[Vertex]] = Set.empty[Edge[Vertex]]
  // ) = DirectedGraph(V, E)

  def G[Vertex, VD, ED](
    V: Set[Vertex] = Set.empty[Vertex],
    E: Set[Edge[Vertex]] = Set.empty[Edge[Vertex]],
    vd: Map[Vertex, VD] = Map.empty[Vertex, VD],
    ed: Map[Edge[Vertex], ED] = Map.empty[Edge[Vertex], ED]
  ) = DirectedGraphData(V, E, vd, ed)

  def vData[Vertex, VD](data: (Vertex, VD)*) = data.map { case (v, datum) => v -> datum }.toMap
  def eData[Vertex, ED](data: ((Vertex, Vertex), ED)*) = data.map { case ((a, b), datum) => e(a -> b) -> datum }.toMap
}
