# pharg
Simple and Flexible Graph Library in Scala

* Immutable
* Cross Compiled to Scala.js
* Serializable with [upickle](http://www.lihaoyi.com/upickle-pprint/upickle) and [boopickle](https://github.com/ochrons/boopickle)

# Usage

```scala
@ import pharg.DSL._

@ val graph = G(V(1, 2), E(1 -> 2)) 
graph: DirectedGraphData[Int, Nothing, Nothing] = G(V(1, 2), E(1 -> 2))
@ graph.vertices 
res2: Set[Int] = Set(1, 2)
@ graph.edges 
res3: Set[Edge[Int]] = Set(1 -> 2)
@ graph + 5 
res4: DirectedGraphData[Int, Nothing, Nothing] = G(V(1, 2, 5), E(1 -> 2))
@ res4 + Edge(1,5) 
res5: DirectedGraphData[Int, Nothing, Nothing] = G(V(1, 2, 5), E(1 -> 2, 1 -> 5))
@ res5.neighbours(1) 
res6: Set[Int] = Set(2, 5)
@ res5.isConnected 
res7: Boolean = true
@ res5 isIsomorphicTo G(V(4,5,6),E(4 -> 5, 4 -> 6)) 
res8: Boolean = true
@ res5.inducedSubGraph(V(1,2)) 
res9: pharg.DirectedGraphData[Int, Nothing, Nothing] = G(V(1, 2), E(1 -> 2))
@ res5.depthFirstSearch(start = 1, res5.successors).toList 
res10: List[Int] = List(1, 5, 2)
```
