package com.github.madbrain.graphlayout

object OverlapCorrecter {

  def apply[T](graph: Graph[T], initialRanks: Map[T, Int], lines: Map[T, Int]) = {
    implicit def toEdgeOp(edge: Edge[T]) = EdgeOperations(edge, lines)

    Iterator.iterate((initialRanks, 0)) {
      case (ranks, rank) =>
        val nodesByRank = ranks.groupBy(_._2).mapValues(_.keys)
        val nodesToMove = nodesByRank(rank).flatMap(node => {
          val fromLine = lines(node)
          graph.incomings(node)
            .filter(edge => edge.getToSide == Side.BOTTOM)
            .map(edge => lines(edge.from))
            .reduceOption(_ max _)
            .map(toLine => nodesByRank(rank)
              // TODO filter dummy nodes
              .filter(n => fromLine < lines(n) && lines(n) < toLine))
            .getOrElse(Seq())
        }).toSet
        if (nodesToMove.isEmpty) {
          (ranks, rank + 1)
        } else {
          val newRanks = nodesToMove.foldLeft(ranks) { case (r, node) => moveNode(node, r, graph) }
          (newRanks, rank)
        }
    }.takeWhile {
      case (ranks, rank) => rank <= ranks.values.max
    }.toSeq.last._1

  }

  def moveNode[T](node: T, ranks: Map[T, Int], graph: Graph[T]): Map[T, Int] = {
    val newRank = ranks(node) + 1
    graph.outgoings(node)
      .foldLeft(ranks.updated(node, newRank)) { case (newRanks, edge) =>
        if (newRanks(edge.to) == newRank) {
          moveNode(edge.to, newRanks, graph)
        } else {
          newRanks
        }
      }
  }
}
