package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class GraphLayouterSpec extends FlatSpec with Matchers with PropertyChecks with GeometryMatchers {

  implicit val helper = new NodeHelper[String] {
    override def isDummy(node: String) = node.startsWith("N")

    override def createDummy(nodes: Set[String]): String = s"N${nodes.size}"
  }

  val sizeProvider = new SizeProvider[String] {
    override def bounds(node: String): Size = if (helper.isDummy(node))
      Size(0, 0)
    else
      Size(40, 40)

    override def horizontalMargin = 20

    override def verticalMargin = 20
  }

  it should "layout graph without edges and nodes overlap" in {

    new java.io.File("target/tests").mkdirs()

    forAll(GraphBuilder.genGraph) { graph =>
      val context = GraphLayouter.layout(graph, sizeProvider)
      SVGGenerator.generate(context, s"target/tests/test_out_${graph.hashCode}.svg")

      // check nodes/nodes overlap
      context.graph.nodes.zipWithIndex.foreach { case (node, i) =>
        (i + 1).until(context.graph.nodes.size)
          .map(context.graph.nodes)
          .foreach(other => {
            val nodeBounds = context.bounds(node)
            val otherBounds = context.bounds(other)
            nodeBounds should not(overlap(otherBounds))
          })
      }

      // check edges/nodes overlap
      context.edgeMap.keys.foreach(edge => {
        makeSegments(context.getPoints(edge)).foreach(segment => {
          context.graph.nodes.foreach(node => {
            context.bounds(node) should not(overlap(segment))
          })
        })
      })
    }
  }

  def makeSegments(points: Seq[Point]) = {
    1.until(points.size).map(i => Segment(points(i - 1), points(i)))
  }
}
