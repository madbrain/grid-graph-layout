package com.github.madbrain.graphlayout

object GraphLayouter {

  def layout[T](graph: Graph[T], sizeProvider: SizeProvider[T])(implicit nodeHelper: NodeHelper[T]) = {
    val dag = DAGTransformer(graph)
    val ranks = TopologicalSorter(dag)
    val (lines, dummies) = LinesBuilder(dag, ranks)
    val (completeGraph, completeRanks, completeLines, edgeMap) = DummyNodesInserter(dag, ranks, lines, dummies)
    val correctedRanks = OverlapCorrecter(completeGraph, completeRanks, completeLines)
    DrawingContext(completeGraph, correctedRanks, completeLines, edgeMap, sizeProvider)
  }
}
