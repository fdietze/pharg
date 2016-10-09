package pharg

import shapeless._
import shapeless.syntax._
import cats._

import algorithm.Neighbours

package directed {
  trait EdgeLike[A] {
    def in: A
    def out: A
  }
  case class Edge[A](in: A, out: A) extends EdgeLike[A]
  case class Graph[A](vertices: Set[A], edges: Set[Edge[A]])

}

package object directed {
  implicit def graphNeighbours[A]: Neighbours[Graph[A], A] = new Neighbours[Graph[A], A] {
    def neighbours(graph: Graph[A], atom: A) = graph.edges.filter(_.in == atom).map(_.out) ++ graph.edges.filter(_.out == atom).map(_.in)

  }

  implicit val edgeFunctor: Functor[Edge] = new Functor[Edge] {
    def map[A, B](fa: Edge[A])(f: A => B) = fa.copy(in = f(fa.in), out = f(fa.out))
  }

  implicit val graphFunctor: Functor[Graph] = new Functor[Graph] {
    def map[A, B](fa: Graph[A])(f: A => B) = fa.copy(
      vertices = fa.vertices map f,
      edges = fa.edges map (Functor[Edge].map(_)(f))
    )
  }

}

package directed {
  package hyper {
    case class Edge[A](incidents: A*)
    case class Graph[A](vertices: Set[A], edges: Set[Edge[A]])
  }

  package object hyper {

    implicit def graphNeighbours[A]: Neighbours[Graph[A], A] = new Neighbours[Graph[A], A] {
      def neighbours(graph: Graph[A], atom: A) = graph.edges.filter(_.incidents contains atom).flatMap(_.incidents) - atom
    }

  }
}

package generalized.directed {
  package hyper {
    case class Graph[A](incidents: Map[A, Set[A]])
  }
  package object hyper {

    implicit def graphNeighbours[A]: Neighbours[Graph[A], A] = new Neighbours[Graph[A], A] {
      def neighbours(graph: Graph[A], atom: A) = (graph.incidents.values.filter(_ contains atom).flatten.toSet - atom)
    }
  }
}

package typed {
  package directed {
    case class Graph[A, E](vertices: Set[A], edges: Set[E])
  }
  package object directed {
    implicit def graphNeighbours[A, E]: Neighbours[Graph[A, E], A] = new Neighbours[Graph[A, E], A] {
      def neighbours(graph: Graph[A, E], atom: A): Iterable[A] = {
        graph.edges.collect {
          case e: pharg.directed.EdgeLike[A] if e.in == atom => e.out
          case e: pharg.directed.EdgeLike[A] if e.out == atom => e.in
        }
      }
    }
  }
}

package generalized {
  case class Graph[A](atoms: Set[A])
}
package object generalized {
  implicit def graphNeighbours[A]: Neighbours[Graph[A], A] = new Neighbours[Graph[A], A] {
    def neighbours(graph: Graph[A], atom: A): Iterable[A] = {
      graph.atoms.collect {
        case e: pharg.directed.EdgeLike[A] if e.in == atom => e.out
        case e: pharg.directed.EdgeLike[A] if e.out == atom => e.in
      }
    }
  }
}

package algorithm {
  trait Neighbours[G, A] {
    def neighbours(graph: G, atom: A): Iterable[A]
  }
}

package object algorithm {

  def degree[G, A](graph: G, atom: A)(implicit n: Neighbours[G, A]): Int = n.neighbours(graph, atom).size
  // def map[G, A](graph: G, f: (A) => A)(implicit n: Neighbours[G, A]): Int = (graph.vertices map f), graph.edges map (_ map f)
  // def filter[G, A](graph: G, atom: (A) => Boolean)(implicit n: Neighbours[G, A]): Int = n.neighbours(graph, atom).size

  // implicit def generalizedTypedHyperGraphNeighbours[A, V <: HList, M[K,V]](implicit ev: M[A,V]): Neighbours[GeneralizedTypedHyperGraph[A, M], A] = new Neighbours[GeneralizedTypedHyperGraph[A, M], A] {
  //   def neighbours(graph: GeneralizedTypedHyperGraph[A, M], atom: A) = graph.atoms.filter(a => graph.incidents.get(a).get.filter[A] contains atom)
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

  // object Schema {
  //   sealed trait Node
  //   sealed trait Edge
  //   sealed trait HyperEdge extends Node with Edge
  //   case class Graph(vs: Set[Node], es: Set[Edge])

  // }
}
