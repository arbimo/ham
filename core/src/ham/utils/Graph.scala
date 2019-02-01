package ham.utils

import scala.collection.mutable

object Graph {

  /** Tarjan's algorithm for extracting strongly connected components.
    *
    * Given a directed graph, the algorithm outputs a sequence of strongly connected
    * components sorted in topological order.
    * */
  def tarjan[V](
               nodes: Iterable[V],
               edges: V => Iterable[V]): Seq[collection.Set[V]] = {
    class Data(var index: Int, var lowLink: Int, var onStack: Boolean)
    var index = 0
    val stack = mutable.ArrayBuffer[V]()
    val data = mutable.Map[V, Data]()


    @inline def pop(stack: mutable.ArrayBuffer[V]): V = {
      stack.remove(stack.length - 1)
    }

    val components: mutable.ArrayBuffer[mutable.Set[V]] = mutable.ArrayBuffer()

    for(v <- nodes) {
      if(!data.contains(v))
        strongConnect(v)
    }

    def strongConnect(v: V): Unit = {
      assert(!data.contains(v))
      data(v) = new Data(index, index, true)
      index += 1
      stack += v
      for(w <- edges(v)) { // todo: creating an object that is not needed
        if(!data.contains(w)) {
          strongConnect(w)
          data(v).lowLink = data(v).lowLink.min(data(w).lowLink)
        } else if(data(w).onStack) {
          data(v).lowLink = data(v).lowLink.min(data(w).index)
        }
      }

      if(data(v).lowLink == data(v).index) {
        val scc = mutable.Set[V]()
        var w = stack.last
        do {
          w = pop(stack)
          data(w).onStack = false
          scc += w
        } while(w != v)
        components += scc
      }
    }

    components.reverse
  }

  case class Cycle[A](nodes: collection.Set[A])

  def topologicalOrder[V](nodes: Iterable[V],
               edges: V => Iterable[V]): Either[Cycle[V], Seq[V]] = {
    val sccs = tarjan(nodes, edges)

    val order = mutable.ArrayBuffer[V]()

    var i = 0
    while(i < sccs.length) {
      if(sccs(i).size == 1)
        sccs(i).foreach(e => order += e)
      else
        return Left(Cycle(sccs(i))) // the graph has a (non-singleton) strongly connected component
      i += 1
    }
    Right(order)
  }


}
