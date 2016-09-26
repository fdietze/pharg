package pharg.generic

import pharg.generic._

class GenericGraphSpec extends org.specs2.mutable.Specification {
  "Typed Directed Graph" >> {
    "degree" >> {
      sealed trait MyTypedVertex
      case class Post(id: Int) extends MyTypedVertex
      case class Author(id: Int) extends MyTypedVertex

      sealed trait MyTypedEdge { def in: MyTypedVertex; def out: MyTypedVertex }
      case class Writes(in: Author, out: Post) extends MyTypedEdge

      implicit def myEdge: DirectedEdge[MyTypedEdge, MyTypedVertex] = new DirectedEdge[MyTypedEdge, MyTypedVertex] {
        def in(e: MyTypedEdge) = e.in
        def out(e: MyTypedEdge) = e.out
      }

      val tg = TypedGraph(
        Seq[MyTypedVertex](Post(1), Post(2), Author(1)),
        Seq[MyTypedEdge](Writes(Author(1), Post(1)), Writes(Author(1), Post(2)))
      )
      degree(tg, Author(1): MyTypedVertex) mustEqual 2
    }
  }

  "Directed Graph" >> {
    val g = Graph(
      Set(1, 2),
      Set(Edge(1, 2))
    )
    degree(g, 1) mustEqual 1
    degree(g, 1) mustEqual 1
    degree(g, 1) mustEqual 1
    degree(g, 1) mustEqual 1
    degree(g, 1) mustEqual 1
  }

  "Hypergraph" >> {
    val hg = HyperGraph(Set(1, 2, 3), Set(HyperEdge(1, 2, 3), HyperEdge(2, 3)))
    degree(hg, 2) mustEqual 2
  }

  "Generalized Hypergraph" >> {
    val hg = GeneralizedHyperGraph(Set(1, 2, 3, 4), Map(2 -> Set(1, 3), 4 -> Set(1, 2)))
    degree(hg, 2) mustEqual 1
  }
}
