package com.github.madbrain.graphlayout

import com.github.madbrain.graphlayout.DummyNodesInserter.DummyNodeInsertion

object LinesBuilder {

  def apply[T](graph: Graph[T], ranks: Map[T, Int]): (Map[T, Int], Seq[DummyNodeInsertion[T]]) = {

    def hasEdgeOnRight(node: T, e: Edge[T], thisLine: Int, lines: Map[T, Int]): Boolean = {
      graph.outgoings(node).foldLeft(false) { case (r, edge) =>
        r | edge != e && lines.get(edge.to).exists(otherLine => thisLine >= otherLine)
      }
    }

    def hasOtherNodeOnLine(node: T, e: Edge[T], thisLine: Int, lines: Map[T, Int]): Boolean = {
      graph.outgoings(node).foldLeft(false) { case (r, edge) =>
        r | edge != e && lines.get(edge.to).contains(thisLine)
      }
    }

    def iterateTree(node: T,
                    initialLines: Map[T, Int],
                    initialLine: Int,
                    initialDummies: Seq[DummyNodeInsertion[T]]): (Map[T, Int], Seq[DummyNodeInsertion[T]], Int) = {
      graph.outgoings(node).foldLeft((initialLines + (node -> initialLine), initialDummies, initialLine)) {
        case ((lines, dummies, line), edge) =>
          if (!lines.contains(edge.to)) {
            if (hasEdgeOnRight(node, edge, line, lines)) {
              iterateTree(edge.to, lines, line + 1, dummies)
            } else {
              iterateTree(edge.to, lines, line, dummies)
            }
          } else {
            if (hasOtherNodeOnLine(node, edge, lines(edge.from), lines)) {
              val dummyLine = line + 1
              val dummy = DummyNodeInsertion(edge, ranks(node) + 1, dummyLine)
              (lines, dummies :+ dummy, dummyLine)
            } else {
              (lines, dummies, line)
            }
          }
      }
    }

    def iterateSource(nodes: Seq[T],
                      lines: Map[T, Int],
                      dummies: Seq[DummyNodeInsertion[T]],
                      line: Int): (Map[T, Int], Seq[DummyNodeInsertion[T]]) = {
      nodes match {
        case head :: tail =>
          val (newLines, newDummies, newLine) = iterateTree(head, lines, line, dummies)
          iterateSource(tail, newLines, newDummies, newLine + 1)
        case _ =>
          (lines, dummies)
      }
    }

    iterateSource(graph.sources.toList, Map[T, Int](), Seq(), 0)
  }
}
