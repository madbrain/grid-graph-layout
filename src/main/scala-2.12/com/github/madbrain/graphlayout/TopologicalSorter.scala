package com.github.madbrain.graphlayout

object TopologicalSorter {

  def apply[T](graph: Graph[T]): Map[T, Int] = {
    def recursiveWalk(rank: Int, currents: Set[T], result: Map[T, Int]): Map[T, Int] = {
      val m = currents.foldLeft(result) { case (r, n) => r.updated(n, rank)}
      val nexts = currents.flatMap(node => graph.outgoings(node)).map(edge => edge.to)
      if (nexts.isEmpty) {
        m
      } else {
        recursiveWalk(rank + 1, nexts, m)
      }
    }
    recursiveWalk(0, graph.sources.toSet, Map())
  }
}
