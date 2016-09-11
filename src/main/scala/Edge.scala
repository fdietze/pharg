package pharg

case class Edge[Vertex](in: Vertex, out: Vertex) {
  assert(in != out, "Self loops are not allowed")

  def contains(v: Vertex) = in == v || out == v
  def toSet = Set(in, out)
  def map(vm: Vertex => Vertex) = Edge(vm(in), vm(out))

  override def toString = s"${in} -> ${out}"
}
