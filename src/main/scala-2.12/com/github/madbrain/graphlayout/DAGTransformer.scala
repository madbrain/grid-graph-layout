package com.github.madbrain.graphlayout

import com.github.madbrain.graphlayout.NodeSelectors.{MaxOutputNodeSelector, SourceNodeSelector}

object DAGTransformer {
  def apply[T](graph: Graph[T]): Graph[T] = {
    val backEdges = FeedbackArcFinder(graph, new SourceNodeSelector(new MaxOutputNodeSelector))
    val dag = Graph(graph.nodes, graph.edges.map(edge => {
      if (backEdges.contains(edge)) {
        ReversedEdge(edge.to, edge.from, edge)
      } else {
        edge
      }
    }))
    dag
  }
}
