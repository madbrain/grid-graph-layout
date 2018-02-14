package com.github.madbrain.graphlayout

import java.io.PrintWriter

object SVGGenerator {

  def generate(context: DrawingContext[String])(implicit helper: NodeHelper[String]) = {

    val printer = new PrintWriter("out.svg", "UTF-8")

    printer.println(
      s"""
         |<svg xmlns='http://www.w3.org/2000/svg' width='${context.width}' height='${context.height}'>
         |<defs>
         |  <marker id="markerArrow" markerWidth="4" markerHeight="4" refX="4" refY="2" orient="auto" markerUnits="strokeWidth">
         |    <path d="M0,0 L0,4 L4,2 z" style="fill: purple;" />
         |  </marker>
         |</defs>
       """.stripMargin)

    context.edgeMap.keys.foreach(edge => {
      val points = context.getPoints(edge)
      printer.println(s"<path d='${makeRoundedLineData(points)}' fill='none' stroke='purple' stroke-width='2px' style='marker-end: url(#markerArrow)' />")
    })

    context.graph.nodes.foreach(node => {
      if (!helper.isDummy(node)) {
        val bounds = context.bounds(node)
        printer.println(s"<g transform='translate(${bounds.x}, ${bounds.y})'>")
        printer.println(s"<rect width='${bounds.width}' height='${bounds.height}' fill='white' stroke='black'/>")
        printer.println(s"<text font-family='helvetica' dx='${bounds.width / 2 + 4}' dy='${bounds.height / 2 + 4}' text-anchor='middle'>$node</text>")
        printer.println("</g>")
      }
    })

    printer.println("</svg>")

    printer.close()
  }

  private def makeRoundedLineData(points: Seq[Point]): String = {
    points.indices.foldLeft("") { case (path, i) =>
      val point = points(i)
      if (i == 0) {
        s"M${point.x},${point.y}"
      } else if (i < points.size-1) {
        val previous = points(i-1)
        val dx = scala.math.signum(point.x - previous.x) * 10
        val dy = scala.math.signum(point.y - previous.y) * 10
        s"$path L${point.x-dx},${point.y-dy} Q${point.x},${point.y},${point.x+dy},${point.y-dx}"
      } else {
        s"$path L${point.x},${point.y}"
      }
    }
  }
}
