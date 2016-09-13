package pharg

import cats.Functor

case class Edge[Vertex](in: Vertex, out: Vertex) {
  assert(in != out, "Self loops are not allowed")

  def contains(v: Vertex) = in == v || out == v
  def toSet = Set(in, out)

  override def toString = s"${in} -> ${out}"
}

object Edge {
  implicit val edgeFunctor: Functor[Edge] = new Functor[Edge] {
    def map[A, B](fa: Edge[A])(f: A => B) = fa match { case Edge(in, out) => Edge(f(in), f(out)) }
  }
}
