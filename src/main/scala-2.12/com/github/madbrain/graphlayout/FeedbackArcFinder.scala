package com.github.madbrain.graphlayout

class FeedbackArcFinder[T](val graph: Graph[T],
                           val nodeSelector: NodeSelector[T]) {

  def find(): Set[Edge[T]] = {
    visitRoot(graph.nodes.toSet, Set[T](), Set[Edge[T]]())
  }

  private def visitRoot(remaining: Set[T],
                             visited: Set[T],
                             backEdges: Set[Edge[T]]): Set[Edge[T]] = {
    if (remaining.isEmpty) {
      backEdges
    } else {
      val (be, v) = visitTree(nodeSelector.select(graph, remaining), Set(), visited, backEdges)
      visitRoot(remaining -- v, v, be)
    }
  }

  private def visitTree(node: T,
                       visiting: Set[T],
                       visited: Set[T],
                       backEdges: Set[Edge[T]]): (Set[Edge[T]], Set[T]) = {
    val outEdges = graph.outgoings(node)
    if (visited.contains(node) || outEdges.isEmpty) {
      (backEdges, visited + node)
    } else {
      outEdges.foldLeft((backEdges, visited)) { case ((be, v), edge) =>
        if (visiting.contains(edge.to)) {
          (be + edge, v + node)
        } else {
          val (nbe, nv) = visitTree(edge.to, visiting + node, v, be)
          (nbe, nv + node)
        }
      }
    }
  }
}

object FeedbackArcFinder {
  def apply[T](graph: Graph[T], nodeSelector: NodeSelector[T]): Set[Edge[T]] = {
    new FeedbackArcFinder[T](graph, nodeSelector).find()
  }
}

trait NodeSelector[T] {
  def select(graph: Graph[T], nodes: Set[T]): T
}

object NodeSelectors {

  class MaxOutputNodeSelector[T] extends NodeSelector[T] {
    def select(graph: Graph[T], nodes: Set[T]): T = {
      nodes.foldLeft((Option.empty[T], -1)) { case ((max, value), node) =>
        max match {
          case None => (Some(node), value)
          case Some(_) =>
            val v = graph.outgoings(node).size - graph.incomings(node).size
            if (v > value) {
              (Some(node), v)
            } else {
              (max, value)
            }
        }
      }._1.getOrElse(throw new RuntimeException("collection cannot be empty"))
    }
  }

  class SourceNodeSelector[T](other: NodeSelector[T]) extends NodeSelector[T] {
    def select(graph: Graph[T], nodes: Set[T]): T = {
      nodes.find(node => graph.incomings(node).isEmpty).getOrElse(other.select(graph, nodes))
    }
  }

}