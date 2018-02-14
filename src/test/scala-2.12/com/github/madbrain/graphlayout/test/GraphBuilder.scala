package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout.{Graph, NormalEdge, Edge}
import org.scalacheck.Gen

import scala.util.Random

object GraphBuilder {

  def buildGraph(n: Int, m: Int) = {
    var nodes = Seq[String]("0")
    var edges = Seq[Edge[String]]()
    while (nodes.size < n) {
      val node = nodes.size.toString
      val other = nodes(Random.nextInt(nodes.size))
      edges = edges :+ NormalEdge(node, other)
      nodes = nodes :+ node
    }
    while (edges.size < m) {
      val n1 = nodes(Random.nextInt(nodes.size))
      val n2 = nodes(Random.nextInt(nodes.size))
      val edge = NormalEdge(n1, n2)
      if (n1 != n2 && !edges.contains(edge)) {
        edges = edges :+ edge
      }
    }
    Graph(nodes, edges)
  }

  val genGraph = for {
    n <- Gen.choose(10, 20)
    m <- Gen.choose(n*2, n*3)
  } yield {
    buildGraph(n, m)
  }
}
