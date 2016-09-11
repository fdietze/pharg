# pharg
Simple and Flexible Graph Library in Scala

# Usage

```scala
@ import pharg._
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
```
