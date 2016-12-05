package pharg

import pharg.DSL._
import collection.immutable.{ListSet, ListMap}

import cats.Functor

class GraphSpec extends org.specs2.mutable.Specification {
  // ListSet and LitsMap preserve the order and makes toString tests reliable
  // note: ListSet in Scala 2.11 and 2.12 have a reversed order
  def V(range: Range) = ListSet(range: _*)
  def V[Vertex](vs: Vertex*) = ListSet(vs: _*)
  def e[Vertex](edge: (Vertex, Vertex)) = edge match { case (in, out) => Edge(in, out) }
  def E[Vertex](es: (Vertex, Vertex)*) = ListSet(es.map { case (in, out) => Edge(in, out) }: _*)
  def vData[Vertex, VD](data: (Vertex, VD)*) = ListMap(data.map { case (v, datum) => v -> datum }: _*)
  def eData[Vertex, ED](data: ((Vertex, Vertex), ED)*) = ListMap(data.map { case ((a, b), datum) => e(a -> b) -> datum }: _*)

  "dsl" >> {
    "vertex set" >> {
      V(1, 2) mustEqual Set(1, 2)
      V() mustEqual Set.empty
    }
    "edge" >> {
      e(3 -> 2) mustEqual Edge(3, 2)
    }
    "edge set" >> {
      E(1 -> 2, 2 -> 3) mustEqual Set(Edge(1, 2), Edge(2, 3))
      E() mustEqual Set.empty
    }
    "vertex-data" >> {
      vData(1 -> "hans", 2 -> "peter") mustEqual Map(1 -> "hans", 2 -> "peter")
    }
    "edge-data" >> {
      eData((1 -> 2) -> "wurst", (2 -> 1) -> "fach") mustEqual Map(Edge(1, 2) -> "wurst", Edge(2, 1) -> "fach")
    }
  }

  "edge" >> {
    "toString" >> {
      e(2 -> 3).toString mustEqual "2 -> 3"
    }
    // "disallow self loops" >> {
    //   e(1 -> 1) must throwAn[AssertionError]
    // }
    "contains vertex" >> {
      (e(2 -> 3) contains 1) must beFalse
      (e(2 -> 3) contains 2) must beTrue
      (e(2 -> 3) contains 3) must beTrue
      (e(2 -> 3) contains 4) must beFalse
    }
    "toSet" >> {
      e(1 -> 2).toSet mustEqual V(1, 2)
    }
    "map" >> {
      Functor[Edge].map(e(1 -> 2))(_ + 1) mustEqual e(2 -> 3)
    }
  }

  "graph" >> {
    "toString" >> {
      "simple" >> {
        G(V(0 to 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3)).toString mustEqual
          "G(V(0, 1, 2, 3, 4, 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))"
      }
      "with vertexData" >> {
        G(V(0 to 1), E(1 -> 0), vData(0 -> "wurst", 1 -> "katapult")).toString mustEqual "G(V(0, 1), E(1 -> 0), {0: wurst, 1: katapult})"
      }
      "with edgeData" >> {
        G(V(0 to 1), E(1 -> 0, 0 -> 1), ed = eData((1 -> 0) -> "kanone", (0 -> 1) -> "salat")).toString mustEqual
          "G(V(0, 1), E(1 -> 0, 0 -> 1), {1->0: kanone, 0->1: salat})"
      }
      "with both data" >> {
        G(V(0 to 1), E(1 -> 0, 0 -> 1),
          vData(0 -> "wurst", 1 -> "katapult"),
          eData((1 -> 0) -> "kanone", (0 -> 1) -> "salat")).toString mustEqual
          "G(V(0, 1), E(1 -> 0, 0 -> 1), {0: wurst, 1: katapult}, {1->0: kanone, 0->1: salat})"
      }
    }
    "map" >> {
      val g = G(V(0 to 3), E(0 -> 1, 1 -> 2, 2 -> 3), vData(0 -> "friedrich", 3 -> "friedhelm"), eData((0 -> 1) -> 0, (2 -> 3) -> 1))
      g.mapVertices(_ + 1) mustEqual G(V(1 to 4), E(1 -> 2, 2 -> 3, 3 -> 4), vData(1 -> "friedrich", 4 -> "friedhelm"), eData((1 -> 2) -> 0, (3 -> 4) -> 1))
    }

    // "assertions" >> {
    //   "edges can only connect existing vertices" >> {
    //     G(V(1), E(1 -> 2)) must throwAn[AssertionError]
    //     G(V(2), E(1 -> 2)) must throwAn[AssertionError]
    //   }
    //   "vertex data can only be attached to existing vertices" >> {
    //     G(V(), vd = vData(1 -> 5)) must throwAn[AssertionError]
    //   }
    //   "edge data can only be attached to existing edges" >> {
    //     G(V(1, 2), ed = eData((1 -> 2) -> 5)) must throwAn[AssertionError]
    //     G(V(), ed = eData((1 -> 2) -> 5)) must throwAn[AssertionError]
    //   }
    // }

    "subGraphOf" >> {
      (G() subGraphOf G()) must beTrue
      (G() subGraphOf G(V(1, 2), E(1 -> 2))) must beTrue
      (G(V(1)) subGraphOf G(V(1, 2), E(1 -> 2))) must beTrue
      (G(V(1, 2), E(1 -> 2)) subGraphOf G(V(1, 2), E(1 -> 2))) must beTrue
      (G(V(1, 2)) subGraphOf G(V(1, 2))) must beTrue
      (G(V(1)) subGraphOf G(V(1, 2))) must beTrue
      (G(V(1, 2, 3)) subGraphOf G(V(1, 2, 3))) must beTrue
      (G(V(1 to 4)) subGraphOf G(V(1 to 4))) must beTrue

      (G(V(3)) subGraphOf G(V(1, 2), E(1 -> 2))) must beFalse
      (G(V(1, 2), E(2 -> 1)) subGraphOf G(V(1, 2), E(1 -> 2))) must beFalse
    }

    "traversal accessors" >> {
      val g = G(
        V(0 to 6),
        E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3),
        vData(2 -> "a", 1 -> "b", 3 -> "c"),
        eData((2 -> 3) -> "x", (1 -> 2) -> "y", (1 -> 0) -> "z")
      )
      "successors" >> {
        g.successors(0) mustEqual V()
        g.successors(1) mustEqual V(0, 2)
        g.successors(2) mustEqual V(4, 3)
        g.successors(3) mustEqual V(5)
        g.successors(4) mustEqual V()
        g.successors(5) mustEqual V(3)
        g.successors(6) mustEqual V()
        // g.successors(7) must throwAn[AssertionError]
      }

      "predecessors" >> {
        g.predecessors(0) mustEqual V(1)
        g.predecessors(1) mustEqual V()
        g.predecessors(2) mustEqual V(1)
        g.predecessors(3) mustEqual V(2, 5)
        g.predecessors(4) mustEqual V(2)
        g.predecessors(5) mustEqual V(3)
        g.predecessors(6) mustEqual V()
        // g.predecessors(7) must throwAn[AssertionError]
      }

      "incoming edges" >> {
        g.incomingEdges(0) mustEqual E(1 -> 0)
        g.incomingEdges(1) mustEqual E()
        g.incomingEdges(2) mustEqual E(1 -> 2)
        g.incomingEdges(3) mustEqual E(2 -> 3, 5 -> 3)
        g.incomingEdges(4) mustEqual E(2 -> 4)
        g.incomingEdges(5) mustEqual E(3 -> 5)
        g.incomingEdges(6) mustEqual E()
        // g.incomingEdges(7) must throwAn[AssertionError]
      }

      "outgoing edges" >> {
        g.outgoingEdges(0) mustEqual E()
        g.outgoingEdges(1) mustEqual E(1 -> 0, 1 -> 2)
        g.outgoingEdges(2) mustEqual E(2 -> 4, 2 -> 3)
        g.outgoingEdges(3) mustEqual E(3 -> 5)
        g.outgoingEdges(4) mustEqual E()
        g.outgoingEdges(5) mustEqual E(5 -> 3)
        g.outgoingEdges(6) mustEqual E()
        // g.outgoingEdges(7) must throwAn[AssertionError]
      }

      "neighbours" >> {
        "over edges" >> {
          g.neighbours(0) mustEqual V(1)
          g.neighbours(1) mustEqual V(0, 2)
          g.neighbours(2) mustEqual V(1, 4, 3)
          g.neighbours(3) mustEqual V(5, 2)
          g.neighbours(4) mustEqual V(2)
          g.neighbours(5) mustEqual V(3)
          g.neighbours(6) mustEqual V()
          // g.neighbours(7) must throwAn[AssertionError]
        }

        "over edges form multiple vertices" >> {
          g.neighbours(V(0)) mustEqual V(1)
          g.neighbours(V(1)) mustEqual V(0, 2)
          g.neighbours(V(2)) mustEqual V(1, 4, 3)
          g.neighbours(V(3)) mustEqual V(5, 2)
          g.neighbours(V(4)) mustEqual V(2)
          g.neighbours(V(5)) mustEqual V(3)
          g.neighbours(V(6)) mustEqual V()

          g.neighbours(V[Int]()) mustEqual V()
          g.neighbours(V(1, 2)) mustEqual V(0, 4, 3)
          g.neighbours(V(5, 3)) mustEqual V(2)
          g.neighbours(V(6, 2, 3, 4)) mustEqual V(5, 1)
          g.neighbours(g.vertices) mustEqual V()
        }
      }

      "incident edges" >> {
        "singe vertex" >> {
          g.incidentEdges(0) mustEqual E(1 -> 0)
          g.incidentEdges(1) mustEqual E(1 -> 0, 1 -> 2)
          g.incidentEdges(2) mustEqual E(1 -> 2, 2 -> 4, 2 -> 3)
          g.incidentEdges(3) mustEqual E(2 -> 3, 5 -> 3, 3 -> 5)
          g.incidentEdges(4) mustEqual E(2 -> 4)
          g.incidentEdges(5) mustEqual E(3 -> 5, 5 -> 3)
          g.incidentEdges(6) mustEqual E()
          // g.incidentEdges(7) must throwAn[AssertionError]

        }
        "multiple vertices" >> {
          g.incidentEdges(V(0)) mustEqual E(1 -> 0)
          g.incidentEdges(V(1)) mustEqual E(1 -> 0, 1 -> 2)
          g.incidentEdges(V(2)) mustEqual E(1 -> 2, 2 -> 4, 2 -> 3)
          g.incidentEdges(V(3)) mustEqual E(2 -> 3, 5 -> 3, 3 -> 5)
          g.incidentEdges(V(4)) mustEqual E(2 -> 4)
          g.incidentEdges(V(5)) mustEqual E(3 -> 5, 5 -> 3)
          g.incidentEdges(V(6)) mustEqual E()

          g.incidentEdges(V[Int]()) mustEqual E()
          g.incidentEdges(V(2, 3)) mustEqual E(1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3)
          g.incidentEdges(V(0, 1)) mustEqual E(1 -> 0, 1 -> 2)
          g.incidentEdges(g.vertices) mustEqual g.edges
        }
      }

      "induced" >> {
        "edges" >> {
          g.inducedEdges(V(0)) mustEqual E()
          g.inducedEdges(V(1)) mustEqual E()
          g.inducedEdges(V(2)) mustEqual E()
          g.inducedEdges(V(3)) mustEqual E()
          g.inducedEdges(V(4)) mustEqual E()
          g.inducedEdges(V(5)) mustEqual E()
          g.inducedEdges(V(6)) mustEqual E()

          g.inducedEdges(V()) mustEqual E()
          g.inducedEdges(V(2, 3)) mustEqual E(2 -> 3)
          g.inducedEdges(V(0, 1)) mustEqual E(1 -> 0)
          g.inducedEdges(V(0, 1, 2, 6)) mustEqual E(1 -> 0, 1 -> 2)
          g.inducedEdges(V(5, 3)) mustEqual E(5 -> 3, 3 -> 5)
          g.inducedEdges(g.vertices) mustEqual g.edges
        }
        "subgraph" >> {
          g.inducedSubGraph(V()) mustEqual G()

          val sub = g.inducedSubGraph(V(5, 3, 2))
          sub.vertices mustEqual V(5, 3, 2)
          sub.edges mustEqual E(2 -> 3, 3 -> 5, 5 -> 3)

          g.inducedSubGraph(g.vertices) mustEqual g
        }.pendingUntilFixed
      }
    }

    "metrics" >> {
      val g = G(V(0 to 6), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
      "inDegree" >> {
        g.inDegree(0) mustEqual 1
        g.inDegree(1) mustEqual 0
        g.inDegree(2) mustEqual 1
        g.inDegree(3) mustEqual 2
        g.inDegree(4) mustEqual 1
        g.inDegree(5) mustEqual 1
        g.inDegree(6) mustEqual 0
        // g.inDegree(7) must throwAn[AssertionError]
      }
      "outDegree" >> {
        g.outDegree(0) mustEqual 0
        g.outDegree(1) mustEqual 2
        g.outDegree(2) mustEqual 2
        g.outDegree(3) mustEqual 1
        g.outDegree(4) mustEqual 0
        g.outDegree(5) mustEqual 1
        g.outDegree(6) mustEqual 0
        // g.outDegree(7) must throwAn[AssertionError]
      }
      "degree" >> {
        g.degree(0) mustEqual 1
        g.degree(1) mustEqual 2
        g.degree(2) mustEqual 3
        g.degree(3) mustEqual 3
        g.degree(4) mustEqual 1
        g.degree(5) mustEqual 2
        g.degree(6) mustEqual 0
        // g.degree(7) must throwAn[AssertionError]
      }

      "isComplete" >> {
        G(V(), E()).isComplete must beTrue
        G(V(1), E()).isComplete must beTrue
        G(V(1, 2), E()).isComplete must beFalse
        G(V(1, 2), E(1 -> 2)).isComplete must beFalse
        G(V(1, 2), E(1 -> 2, 2 -> 1)).isComplete must beTrue
      }

      "isEmpty" >> {
        G(V(), E()).isEmpty must beTrue
        G(V(1), E()).isEmpty must beFalse
        G(V(1, 2), E()).isEmpty must beFalse
        G(V(1, 2), E(1 -> 2)).isEmpty must beFalse
      }

      "isConnected" >> {
        G(V(), E()).isConnected must beTrue
        G(V(1), E()).isConnected must beTrue
        G(V(1, 2), E()).isConnected must beFalse
        G(V(1, 2), E(1 -> 2)).isConnected must beTrue
        G(V(1, 2, 3), E(1 -> 2)).isConnected must beFalse
      }
    }

    "modifications" >> {
      "add vertex" >> {
        // "existing" >> {
        //   G(V(0 to 2)) + 1 must throwAn[AssertionError]
        // }
        "nonexisting" >> {
          G(V(0 to 2)) + 3 mustEqual G(V(0 to 3))
        }
      }

      "add edge" >> {
        // "existing" >> {
        //   G(V(0 to 2), E(1 -> 2)) + e(1 -> 2) must throwAn[AssertionError]
        // }
        "nonexisting" >> {
          G(V(0 to 2), E(1 -> 2)) + e(0 -> 2) mustEqual G(V(0 to 2), E(1 -> 2, 0 -> 2))
        }
      }

      val g = G(
        V(0 to 4),
        E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
        vData(3 -> "a", 4 -> "b"),
        eData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L)
      )

      "remove vertex" >> {
        // "from empty graph" >> {
        //   G() - 1 must throwA[AssertionError]
        // }
        // "nonexisting vertex" >> {
        //   g - 17 must throwA[AssertionError]
        // }
        "existing vertex with edges and vertexData" >> {
          g - 4 mustEqual G(
            V(0 to 3),
            E(0 -> 3, 1 -> 3),
            vData(3 -> "a"),
            eData((1 -> 3) -> 17L, (0 -> 3) -> -15L)
          )
        }

        "existing vertex with edgedata" >> {
          g - 3 mustEqual G(
            V(0, 1, 2, 4),
            E(0 -> 4, 4 -> 2),
            vData(4 -> "b"),
            eData(0 -> 4 -> 18L)
          )
        }
      }

      "remove edge" >> {
        // "from empty graph" >> {
        //   G[Int, Nothing, Nothing]() - e(1 -> 2) must throwA[AssertionError]
        // }
        // "nonexisting edge" >> {
        //   g - e(2 -> 4) must throwA[AssertionError]
        // }

        "existing edge" >> {
          g - e(4 -> 2) mustEqual G(
            V(0 to 4),
            E(0 -> 3, 1 -> 3, 0 -> 4),
            vData(3 -> "a", 4 -> "b"),
            eData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L)
          )
        }

        "existing edge with data" >> {
          g - e(0 -> 3) mustEqual G(
            V(0 to 4),
            E(1 -> 3, 0 -> 4, 4 -> 2),
            vData(3 -> "a", 4 -> "b"),
            eData((1 -> 3) -> 17L, 0 -> 4 -> 18L)
          )
        }
      }

      "merge graphs" >> {
        "example" >> {
          val g = G(
            V(0 to 2),
            E(0 -> 1, 1 -> 2),
            vData(0 -> 0, 2 -> 0),
            eData((0 -> 1) -> "A", (1 -> 2) -> "A")
          )
          val g2 = G(
            V(1 to 3),
            E(2 -> 1, 3 -> 2, 1 -> 2),
            vData(2 -> 1, 3 -> 1),
            eData((1 -> 2) -> "B")
          )
          g ++ g2 mustEqual G(V(0 to 3), E(0 -> 1, 1 -> 2, 2 -> 1, 3 -> 2), vData(0 -> 0, 2 -> 1, 3 -> 1), eData((0 -> 1) -> "A", (1 -> 2) -> "B"))
        }
      }

      "remove subgraph" >> {
        // "assertions" >> {
        //   g -- G(V(0 to 17)) must throwAn[AssertionError]
        // }
        "full example" >> {
          val g = G[Int, Nothing, Nothing](
            V(0 to 4),
            E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
            Map.empty,
            Map.empty
          )
          g -- G(V(1 to 3)) mustEqual G(V(0, 4), E(0 -> 4))
        }
        "with data" >> {
          val g = G(
            V(0 to 4),
            E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4),
            vData(0 -> 1, 4 -> 2),
            eData((0 -> 1) -> "A", (3 -> 4) -> "B")
          )
          g -- G(V(2 to 4)) mustEqual G(V(0 to 1), E(0 -> 1), vData(0 -> 1), eData((0 -> 1) -> "A"))
        }
      }
    }

    "detect cycle" >> {
      "no cycle" >> {
        val g = G(V(0 to 5), E(0 -> 1, 0 -> 2, 1 -> 3, 2 -> 4, 2 -> 5))
        g.hasCycle must beFalse
      }
      "undirected cycle" >> {
        val g = G(V(0 to 4), E(0 -> 1, 2 -> 1, 2 -> 3, 3 -> 4, 4 -> 1))
        g.hasCycle must beFalse
      }
      "directed cycle" >> {
        val g = G(V(0 to 4), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1))
        g.hasCycle must beTrue
      }
      "small graph cycle" >> {
        val g = G(V(0 to 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
        g.hasCycle must beTrue
      }
    }

    "depth first search" >> {
      // "nonexistent vertex" >> {
      //   G().depthFirstSearch(17).toList must throwAn[AssertionError]
      // }
      "one vertex" >> {
        val g = G(V(0 to 0))
        g.depthFirstSearch(0).toList mustEqual List(0)
      }
      "directed cycle" >> {
        val g = G(V(0 to 3), E(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2, 3 -> 0, 2 -> 1))
        g.depthFirstSearch(0, v => g.successors(v).toList.sorted.reverse).toList mustEqual List(0, 1, 3, 2)
      }
      "undirected cycle" >> {
        val g = G(V(0 to 3), E(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2))
        g.depthFirstSearch(0, v => g.successors(v).toList.sorted.reverse).toList mustEqual List(0, 1, 3, 2)
        g.depthFirstSearch(0, v => g.successors(v).toList.sorted).toList mustEqual List(0, 2, 1, 3)
      }
    }

    "topological sort" >> {
      G(V(), E()).topologicalSort mustEqual List()
      G(V(1), E()).topologicalSort mustEqual List(1)
      G(V(1, 2), E(1 -> 2)).topologicalSort mustEqual List(1, 2)
      // G(V(1, 2), E(1 -> 2, 2 -> 1)).topologicalSort must throwAn[AssertionError]
      G(V(1, 2, 3), E(2 -> 3, 1 -> 2)).topologicalSort mustEqual List(1, 2, 3)
      val s = G(V(1, 2, 3, 4, 5), E(1 -> 3, 2 -> 3, 3 -> 4, 3 -> 5)).topologicalSort
      List(s.slice(0, 2).toSet, s(2), s.slice(3, 5).toSet) mustEqual List(V(1, 2), 3, V(4, 5))
    }

    "connected components" >> {
      G(V(0, 1, 2), E(0 -> 1, 1 -> 2)).connectedComponents mustEqual Set(V(0, 1, 2))
      G(V(0, 1, 2), E(0 -> 1)).connectedComponents mustEqual Set(V(0, 1), V(2))
      G(V(0 to 5), E(1 -> 2, 2 -> 1, 3 -> 5, 4 -> 5)).connectedComponents mustEqual Set(V(0), V(1, 2), V(3, 4, 5))
    }

    "graph isomorphism" >> {
      (G(V(1)) isIsomorphicTo G(V())) must beFalse
      (G(V(1)) isIsomorphicTo G(V(2))) must beTrue
      (G(V(1, 3)) isIsomorphicTo G(V(2))) must beFalse
      (G(V(1, 3)) isIsomorphicTo G(V(2, 4))) must beTrue
      (G(V(1, 3), E(1 -> 3)) isIsomorphicTo G(V(2, 4))) must beFalse
      (G(V(1, 3), E(1 -> 3)) isIsomorphicTo G(V(2, 4), E(4 -> 2))) must beTrue
      (G(V(1, 3), E(1 -> 3)) isIsomorphicTo G(V(2, 4), E(4 -> 2, 2 -> 4))) must beFalse
      (G(V(1, 3), E(1 -> 3, 3 -> 1)) isIsomorphicTo G(V(2, 4), E(4 -> 2, 2 -> 4))) must beTrue
      (G(V(0 to 4), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4)) isIsomorphicTo G(V(0 to 4), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4))) must beTrue
      (G(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 1 -> 5)) isIsomorphicTo G(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 2 -> 5))) must beFalse
      (G(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 1 -> 0)) isIsomorphicTo G(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 2 -> 0))) must beFalse
    }

  }
}
