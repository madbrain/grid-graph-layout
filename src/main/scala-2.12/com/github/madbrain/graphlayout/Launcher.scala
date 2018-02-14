package com.github.madbrain.graphlayout

import scala.util.Random

object Launcher {

  def main(args: Array[String]) {
    val nodes = 0.to(10).map(_.toString)
    val edges = Seq((1, 0), (2, 1), (3, 1), (4, 0), (5, 0), (6, 0), (7, 3), (8, 3), (9, 2), (10, 1), (8, 0),
      (3, 2), (9, 0), (3, 6), (5, 4), (6, 4), (7, 0), (4, 7), (9, 1), (0, 9), (5, 2), (4, 3), (10, 9),
      (4, 1), (0, 3), (1, 9), (10, 8), (6, 10))
      .map { case (from, to) => NormalEdge(from.toString, to.toString) }
    val graph = Graph(nodes, edges)

    implicit val helper = new NodeHelper[String] {
      override def isDummy(node: String) = node.startsWith("N")
    }

    implicit def dummyNodeCreation(nodes: Set[String]): String = s"N${nodes.size}"

    val sizeProvider = new SizeProvider[String] {
      private val rand = new Random()
      override def bounds(node: String): Size = if (node.startsWith("N"))
        Size(0, 0)
      else
//        Size(40 + rand.nextInt(40), 40 + rand.nextInt(40))
        Size(40, 40)

      override def horizontalMargin = 20

      override def verticalMargin = 20
    }

    val dag = DAGTransformer(graph)
    val ranks = TopologicalSorter(dag)
    val (lines, dummies) = LinesBuilder(dag, ranks)
    val (completeGraph, completeRanks, completeLines, edgeMap) = DummyNodesInserter(dag, ranks, lines, dummies)
    val correctedRanks = OverlapCorrecter(completeGraph, completeRanks, completeLines)

    val context = DrawingContext(completeGraph, correctedRanks, completeLines, edgeMap, sizeProvider)

    SVGGenerator.generate(context)
  }

}
