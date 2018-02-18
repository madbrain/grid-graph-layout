package com.github.madbrain.graphlayout.test

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class GraphBuilderSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "generate fully connected graph" in {
    forAll(GraphBuilder.genGraph) { graph =>
      val connectedNodes = graph.edges.flatMap(edge => Set(edge.from, edge.to)).toSet
      connectedNodes should be(graph.nodes.toSet)
    }
  }
}
