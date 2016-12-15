package pharg

import cats.Functor

case class Edge[+Vertex](in: Vertex, out: Vertex) {
  // assert(in != out, "Self loops are not allowed")

  def contains[V >: Vertex](v: V) = in == v || out == v
  def toSet[V >: Vertex]: Set[V] = Set(in, out)

  override def toString = s"${in} -> ${out}"
}

object Edge {
  implicit val edgeFunctor: Functor[Edge] = new Functor[Edge] {
    def map[A, B](fa: Edge[A])(f: A => B) = fa match { case Edge(in, out) => Edge(f(in), f(out)) }
  }
}
