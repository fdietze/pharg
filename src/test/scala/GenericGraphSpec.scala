package pharg

import pharg.algorithm._
import cats._

// requirements:
//
// directed graph
// undirected graph
// with data on vertices / edges
// with specific vertex types
// with specific edges types, connecting specific vertex types
// edges connecting other edges or vertices
// hypergraph
// directed vs undirected hypergraphs
// - Undirected Hyperedge: (incident:Set[V])
// - Directed Hyperedge: (in:Set[V], out:Set[V])
// - Directed Hyperedge: (incident:Seq[V], i: Interpretation[ich])
// multigraphs
//
// Different Performance characteristics
// - Fast modification
// - Fast traversal and algorithms

class GenericGraphSpec extends org.specs2.mutable.Specification {
  "directed.Graph" >> {
    import pharg.directed._

    val g = Graph(
      Set(1, 2),
      Set(Edge(1, 2))
    )

    "degree" >> {
      degree(g, 1) mustEqual 1
    }

    "map" >> {
      Functor[Graph].map(g)(_ + 1) mustEqual Graph(Set(2, 3), Set(Edge(2, 3)))
    }
  }

  "directed.hyper.Graph" >> {
    import pharg.directed.hyper._

    val g = Graph(Set(1, 2, 3), Set(Edge(1, 2, 3), Edge(2, 3)))

    "degree" >> {
      degree(g, 2) mustEqual 2
    }
  }

  "generalized.directed.hyper.Graph" >> {
    import pharg.generalized.directed.hyper._

    val g = Graph(Map(2 -> Set(1, 3), 4 -> Set(1, 2)))

    "degree" >> {
      degree(g, 2) mustEqual 1
      degree(g, 1) mustEqual 2
      degree(g, 4) mustEqual 0
    }
  }

  "Typed Directed Graph" >> {
    import typed.directed._

    sealed trait MyTypedVertex
    case class Post(id: Int) extends MyTypedVertex
    case class Author(id: Int) extends MyTypedVertex

    sealed trait MyTypedEdge extends directed.EdgeLike[MyTypedVertex] { def in: MyTypedVertex; def out: MyTypedVertex }
    case class Writes(in: Author, out: Post) extends MyTypedEdge

    val g = Graph[MyTypedVertex, MyTypedEdge](
      Set(Post(1), Post(2), Author(1)),
      Set(Writes(Author(1), Post(1)), Writes(Author(1), Post(2)))
    )

    "degree" >> {
      degree(g, Author(1): MyTypedVertex) mustEqual 2
    }

    // "map" >> {
    //   Functor[Graph].map(g) {
    //     case Post(id) => Post(id + 1)
    //     case Author(id) => Author(id * 2)
    //   } mustEqual Graph(Set(2, 3), Set(Edge(2, 3)))
    // }
  }

  "Typed Generalized Hypergraph" >> {
    import generalized._
    sealed trait MyAtom
    sealed trait MyEdge extends MyAtom with pharg.directed.EdgeLike[MyAtom]
    case class Author(name: String) extends MyAtom
    case class Writes(in: Author, out: Post, date: Long) extends MyEdge

    sealed trait Respondee extends MyAtom
    case class Post(title: String) extends Respondee
    case class RespondsTo(in: Post, out: Respondee, date: Long) extends Respondee with MyEdge

    val author = Author("Uwe")
    val post1 = Post("Engel")
    val writes = Writes(author, post1, 0)
    val post2 = Post("Wust")
    val post3 = Post("Ultrakrass")
    val respondsTo1 = RespondsTo(post1, post2, 1)
    val respondsTo2 = RespondsTo(post3, respondsTo1, 2)

    val g = Graph[MyAtom](Set(author, post1, post2, post3, writes, respondsTo1, respondsTo2))

    "degree" >> {
      degree[Graph[MyAtom], MyAtom](g, author) mustEqual 1
      degree[Graph[MyAtom], MyAtom](g, post1) mustEqual 2
      degree[Graph[MyAtom], MyAtom](g, respondsTo1) mustEqual 1
      degree[Graph[MyAtom], MyAtom](g, respondsTo2) mustEqual 0
    }

    // "map" >> {
    //   Functor[Graph].map(g)(
    //     {
    //       case Post(title) => Post(title.toUpperCase)
    //     }.applyOrElse(_, identity)
    //   ) mustEqual Graph(Set(2, 3), Set(Edge(2, 3)))
    // }
  }

  "ER Diagram" >> {
    // The ER Diagram needs to be a generalized graph to model relation inheritance.
    // It is also a multigraph, because inheritance/relationship can connect the same vertices.
    import generalized.multi._

    sealed trait Atom
    sealed trait NamedAtom extends Atom { def name: String }
    sealed trait EntityLike extends NamedAtom
    sealed trait RelationshipLike extends NamedAtom

    sealed trait TraitLike extends Atom
    sealed trait EntityTraitLike extends EntityLike with TraitLike
    sealed trait RelationshipTraitLike extends RelationshipLike with TraitLike

    case class RelationshipTrait(name: String) extends RelationshipTraitLike
    case class EntityTrait(name: String) extends EntityTraitLike
    case class PropertyTrait(name: String, properties: Map[String, String] = Map.empty) extends RelationshipTraitLike with EntityTraitLike

    case class Entity(name: String, properties: Map[String, String] = Map.empty) extends EntityLike
    case class Relationship(in: EntityLike, name: String, out: EntityLike,
      properties: Map[String, String] = Map.empty) extends RelationshipLike with pharg.directed.EdgeLike[Atom]
    case class HyperRelationship(in: EntityLike, name: String, out: EntityLike,
      properties: Map[String, String] = Map.empty) extends EntityLike with RelationshipLike with pharg.directed.EdgeLike[Atom]

    sealed trait Inheritance extends Atom with pharg.directed.EdgeLike[Atom]
    case class EntityInheritance(in: Entity, out: EntityTraitLike) extends Inheritance
    case class EntityTraitInheritance(in: EntityTrait, out: EntityTraitLike) extends Inheritance
    case class RelationshipInheritance(in: Relationship, out: RelationshipTraitLike) extends Inheritance
    case class RelationshipTraitInheritance(in: RelationshipTrait, out: RelationshipTraitLike) extends Inheritance
    case class HyperRelationshipInheritance(in: HyperRelationship, out: TraitLike) extends Inheritance
    case class PropertyTraitInheritance(in: PropertyTrait, out: PropertyTrait) extends Inheritance

    val connectable = EntityTrait("Connectable")
    val post = Entity("Post", Map("title" -> "String"))
    val postIsConnectable = EntityInheritance(post, connectable)
    val user = Entity("User", Map("name" -> "String"))
    val respondsTo = HyperRelationship(post, "RespondsTo", connectable, Map("name" -> "String"))
    val respondsToIsConnectable = HyperRelationshipInheritance(respondsTo, connectable)
    val contributes = RelationshipTrait("Contributes")
    val writes = Relationship(user, "Writes", post)
    val writingIsContributing = RelationshipInheritance(writes, contributes)
    val g = Graph[Atom](Seq(connectable, post, postIsConnectable, user, respondsTo, respondsToIsConnectable, contributes, writes, writingIsContributing))

    degree[Graph[Atom], Atom](g, post) mustEqual 3
  }
}
