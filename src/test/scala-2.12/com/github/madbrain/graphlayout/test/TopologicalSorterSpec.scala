package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout.{TopologicalSorter, DAGTransformer}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FlatSpec}

class TopologicalSorterSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "sort all nodes" in {
    forAll(GraphBuilder.genGraph) { graph =>
      // println(graph)
      val dag = DAGTransformer(graph)
      val ranks = TopologicalSorter(dag)

      dag.edges.foreach(edge => {
        ranks(edge.from) should be < ranks(edge.to)
      })
    }
  }
}
