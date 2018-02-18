package com.github.madbrain.graphlayout

object DummyNodesInserter {

  case class DummyNodeInsertion[T](edge: Edge[T], rank: Int, line: Int)

  def apply[T](graph: Graph[T], ranks: Map[T, Int], lines: Map[T, Int], dummies: Seq[DummyNodeInsertion[T]])
              (implicit nodeHelper: NodeHelper[T]) = {
    val dummiesByEdge = dummies.map(dummy => dummy.edge -> dummy).toMap
    val result = graph.edges.foldLeft((Map[T, Int](), Map[T, Int](), Seq[(Edge[T], Seq[Edge[T]])]())) {
      case ((prevRanks, prevLines, prevEdges), edge) =>
        dummiesByEdge.get(edge) match {
          case Some(dummy) =>
            val newNode = nodeHelper.createDummy(prevRanks.keys.toSet)
            (prevRanks + (newNode -> dummy.rank),
              prevLines + (newNode -> dummy.line),
              prevEdges :+ (edge -> Seq(NormalEdge(dummy.edge.from, newNode), NormalEdge(newNode, dummy.edge.to))))
          case None =>
            (prevRanks, prevLines, prevEdges :+ (edge -> Seq(edge)))
        }
    }
    val completeRanks = ranks ++ result._1
    val completeLines = lines ++ result._2
    val edges = result._3.flatMap { case(edge, newEdges) => newEdges }
    val completeGraph = Graph(graph.nodes ++ result._1.keys, edges)
    (completeGraph, completeRanks, completeLines, result._3.toMap)
  }
}
