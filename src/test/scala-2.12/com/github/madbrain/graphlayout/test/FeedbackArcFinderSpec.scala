package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout.NodeSelectors.{MaxOutputNodeSelector, SourceNodeSelector}
import com.github.madbrain.graphlayout._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FeedbackArcFinderSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "find feedback edges (simple)" in {
    val nodes = 1.to(5).map(_.toString)
    val edges = Seq((1,2), (2,3), (3,4), (4,5), (4,2), (5,3))
      .map { case (from, to) => NormalEdge(from.toString, to.toString) }
    val graph = Graph(nodes, edges)

    val result = FeedbackArcFinder(graph, new SourceNodeSelector(new MaxOutputNodeSelector))

    result should be(Set(NormalEdge("4", "2"), NormalEdge("5", "3")))
  }

  it should "find multiple feedback edges" in {
    forAll(GraphBuilder.genGraph) { graph =>
      // println(graph)
      val backEdges = FeedbackArcFinder(graph, new SourceNodeSelector(new MaxOutputNodeSelector))

      // PROPERTIES graph without backEdges have sources
      // DFS from sources has no cycles
      val g = Graph(graph.nodes, graph.edges.filter(edge => !backEdges.contains(edge)))
      def visit(toVisit: List[String], visiting: Set[String]): Unit = {
        toVisit match {
          case head :: tail =>
            visiting should not contain head
            visit(g.outgoings(head).map(edge => edge.to).toList, visiting + head)
            visit(tail, visiting)
          case _ =>
        }
      }
      visit(g.sources.toList, Set())
    }
  }
}
