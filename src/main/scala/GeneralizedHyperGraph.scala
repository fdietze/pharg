package pharg
// requirements:
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
//
//

package generic {
  case class Edge[A](in: A, out: A)
  case class Graph[A](vertices: Set[A], edges: Set[Edge[A]])

  case class HyperEdge[A](incident: A*)
  case class HyperGraph[A](vertices: Set[A], edges: Set[HyperEdge[A]])

  trait DirectedEdge[E, A] {
    def in(edge: E): A
    def out(edge: E): A
  }

  case class TypedGraph[A, E](vertices: Seq[A], edges: Seq[E])(implicit e: DirectedEdge[E, A])

  case class GeneralizedHyperGraph[A](incidence: Map[A, Set[A]])

  trait Neighbours[G, A] {
    def neighbours(graph: G, atom: A): Set[A]
  }
}

package object generic {

  def degree[G, A](graph: G, atom: A)(implicit n: Neighbours[G, A]): Int = n.neighbours(graph, atom).size

  implicit def graphNeighbours[A]: Neighbours[Graph[A], A] = new Neighbours[Graph[A], A] {
    def neighbours(graph: Graph[A], atom: A) = graph.edges.filter(_.in == atom).map(_.out) ++ graph.edges.filter(_.out == atom).map(_.in)
  }

  implicit def hyperGraphNeighbours[A]: Neighbours[HyperGraph[A], A] = new Neighbours[HyperGraph[A], A] {
    def neighbours(graph: HyperGraph[A], atom: A) = graph.edges.filter(_.incident contains atom).flatMap(_.incident) - atom
  }

  implicit def typedGraphNeighbours[A, E](implicit e: DirectedEdge[E, A]): Neighbours[TypedGraph[A, E], A] = new Neighbours[TypedGraph[A, E], A] {
    def neighbours(graph: TypedGraph[A, E], atom: A) = (
      graph.edges.filter(e.in(_) == atom).map(e.out(_)) ++
      graph.edges.filter(e.out(_) == atom).map(e.in(_))
    ).toSet
  }

  implicit def generalizedHyperGraphNeighbours[A]: Neighbours[GeneralizedHyperGraph[A], A] = new Neighbours[GeneralizedHyperGraph[A], A] {
    def neighbours(graph: GeneralizedHyperGraph[A], atom: A) = (graph.incidence.values.filter(_ contains atom).flatten.toSet - atom)
  }

  // case class Graph(vertices: Set[Vertex], edges: Set[Edge])

  // type Vertex = Atom[HNil]
  // type DirectedEdge = Atom[Vertex :: Vertex :: HNil]
  // type HyperEdge = Atom[Vertex :: Vertex :: HNil] // TODO: arbitrary sized list?
  // type DirectedGraph = GeneralizedHyperGraph[Vertex :+: DirectedEdge :+: CNil]

  // val g: DirectedGraph = GeneralizedHyperGraph(Seq(Atom(HNil), Atom(Atom(HNil), Atom(HNil))))

  // case class Vertex(neighbours: Iterable[Vertex] = Nil)

  // implicit val undirectedVertexNeighbourhood = new Neighbourhood[Vertex] {
  //   def neighbours(atom: Vertex) = atom.neighbours
  // }

  // case class DirectedGraph[Vertex: Neighbourhood](vertices: Set[Vertex])

  // val v1 = Vertex()
  // val v2 = Vertex()
  // val g = DirectedGraph(Set(Vertex()))

  // case class GeneralizedHyperGraph(atoms: Set[Atom])

  // case class Atom[L <: HList](incidents: L) // Type Class
  // case class GeneralizedHyperGraph[C <: Coproduct](atoms: Seq[C] = Nil) //Functor, Monoid

  // case class HyperGraph[A, B](vertices: Set[Atom[A]], edges: Seq[Atom[B]]) // Functor, Monoid

  // trait Node[A] {
  //   def neighbours: Seq[A]
  // }

  object SchemaDSL {
    // @Vertex trait Connectable

    // @Vertex class Post(title: String) extends Connectable
    // @Vertex class User(name: String)

    // @HyperEdge class RespondsTo(start: Post, end: Connectable) extends Connectable
    // @Edge class Authors(date: Long)(start: User, end: Post)
    // @Edge class Authors(start: User, end: Post, date: Long)

    // trait Connectable
    // (:Post:Connectable {title: String})
    // (:User {name: String})
    // (:Post)-(:RespondsTo:Connectable)->(:Connectable)
    // (:User)-[:Authors {date: Long}]->(:Post)
  }

  // object SchemaERDiagram {
  //   // Selbst Die Schema Datenstruktur muss ein Hypergraph sein, um Kantenvererbung darzustellen
  //   sealed trait Entity {def name:String; def properties:Map[String, String], inherits: Seq[VertexTrait]}
  //   sealed trait Relationship
  //   sealed trait Trait extends Entity

  //   case class VertexTrait(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[VertexTrait] = Nil) extends Trait
  //   case class EdgeTrait(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[EdgeTrait] = Nil) extends Trait

  //   case class Vertex(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[VertexTrait] = Nil) extends Entity
  //   case class HyperEdge(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[Trait] = Nil) extends Entity

  //   case class Edge(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[EdgeTrait] = Nil) extends Relationship
  //   case class HyperEdgeStart(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[Trait] = Nil) extends Relationship
  //   case class HyperEdgeEnd(name: String, properties: Map[String, String] = Map.empty, inherits: Seq[Trait] = Nil) extends Relationship
  // }

  // object SchemaDataStructure {
  //   import SchemaERDiagram._

  //   val connectable = Trait("Connectable")
  //   Graph[Entity, Relationship](
  //     Set(
  //       Vertex("Post", Map("title" -> "String"), inherits = Seq(connectable)),
  //       Vertex("User", Map("name" -> "String")),
  //       HyperEdge("RespondsTo", Map("name" -> "String")),

  //     )
  //   )

  // }

  // object Schema {
  //   sealed trait Node
  //   sealed trait Edge
  //   sealed trait HyperEdge extends Node with Edge
  //   case class Graph(vs: Set[Node], es: Set[Edge])

  // }
}
