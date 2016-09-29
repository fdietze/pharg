package pharg.generic

import shapeless._
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
  }

  "Hypergraph" >> {
    val hg = HyperGraph(Set(1, 2, 3), Set(HyperEdge(1, 2, 3), HyperEdge(2, 3)))
    degree(hg, 2) mustEqual 2
  }

  "Generalized Hypergraph" >> {
    val hg = GeneralizedHyperGraph(Map(2 -> Set(1, 3), 4 -> Set(1, 2)))
    degree(hg, 2) mustEqual 1
    degree(hg, 1) mustEqual 2
    degree(hg, 4) mustEqual 0
  }

  "Generalized Typed Hypergraph" >> {
    sealed trait MyAtom
    case class Author(name:String) extends MyAtom
    case class Writes(date:Long) extends MyAtom
    case class Post(title:String) extends MyAtom
    case class RespondsTo(date:Long) extends MyAtom

    class Allowed[K, V]
    implicit val allowAtom = new Allowed[MyAtom, HNil]
    implicit val allowWrites = new Allowed[Writes, Author :: Post :: HNil]
    implicit val allowRespondsToPost = new Allowed[RespondsTo, Post :: Post :: HNil]
    implicit val allowRespondsToRespondsTo = new Allowed[RespondsTo, Post :: RespondsTo :: HNil]

    val author = Author("Uwe")
    val writes = Writes(0)
    val post1 = Post("Engel")
    val post2 = Post("Wust")
    val post3 = Post("Ultrakrass")
    val respondsTo1 = RespondsTo(1)
    val respondsTo2 = RespondsTo(2)

    val g = GeneralizedTypedHyperGraph[MyAtom, Allowed](
      Set(author, writes, post1, post2, post3, respondsTo1, respondsTo2),
      HMap[Allowed](
      writes -> (author :: post1 :: HNil),
      respondsTo1 -> (post1 :: post2 :: HNil),
      respondsTo2 -> (post3 :: respondsTo1 :: HNil)
    ))

      // implicit def myAtom: DirectedHyperEdge[MyTypedEdge, MyTypedVertex] = new DirectedEdge[MyTypedEdge, MyTypedVertex] {
      //   def in(e: MyTypedEdge) = e.in
      //   def out(e: MyTypedEdge) = e.out
      // }

    degree(g, author) mustEqual 1
    // degree(g, post1) mustEqual 2
    // degree(g, respondsTo1) mustEqual 1
    // degree(g, respondsTo2) mustEqual 0
  }
}
