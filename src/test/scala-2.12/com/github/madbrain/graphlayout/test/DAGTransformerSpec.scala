package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout.DAGTransformer
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class DAGTransformerSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "transform graph to DAG" in {
    forAll(GraphBuilder.genGraph) { graph =>
      // println(graph)
      val dag = DAGTransformer(graph)

      // DFS from sources has no cycles
      def visit(toVisit: List[String], visiting: Set[String]): Unit = {
        toVisit match {
          case head :: tail =>
            visiting should not contain head
            visit(dag.outgoings(head).map(edge => edge.to).toList, visiting + head)
            visit(tail, visiting)
          case _ =>
        }
      }
      visit(dag.sources.toList, Set())
    }
  }
}