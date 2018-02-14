package com.github.madbrain.graphlayout

trait SizeProvider[T] {
  def bounds(node: T): Size

  def horizontalMargin: Int

  def verticalMargin: Int
}

case class SideEdgeInfo[T](node: T, side: Side.Side, edges: Seq[Edge[T]], straightEdge: Option[Edge[T]]) {
  def adjust(edge: Edge[T], sizeProvider: Map[T, Rectangle]): Point = {
    def getSize: Int = {
      if (side == Side.LEFT || side == Side.RIGHT)
        sizeProvider(node).height
      else
        sizeProvider(node).width
    }
    val point = getSidePoint(sizeProvider)
    if (edges.size > 1 && !straightEdge.contains(edge)) {
      val (size, pos, total) = straightEdge.foldLeft((getSize, edges.indexOf(edge) + 1, edges.size + 1)) {
        case ((s, p, t), st) =>
          val newSize = s - s / 2
          val straightPos = edges.indexOf(st)
          if (straightPos > p) {
            (newSize, p, straightPos)
          } else {
            (newSize, p - straightPos, t - straightPos)
          }
      }
      val offset = (size * pos) / total - size / 2
      if (side == Side.LEFT || side == Side.RIGHT) {
        Point(point.x, point.y + offset)
      } else {
        Point(point.x + offset, point.y)
      }
    } else {
      point
    }
  }

  def getSidePoint(sizeProvider: Map[T, Rectangle]): Point = {
    val bounds = sizeProvider(node)
    side match {
      case Side.LEFT => Point(bounds.x, bounds.getCenterY)
      case Side.RIGHT => Point(bounds.getMaxX, bounds.getCenterY)
      case Side.TOP => Point(bounds.getCenterX, bounds.y)
      case Side.BOTTOM => Point(bounds.getCenterX, bounds.getMaxY)
    }
  }
}

case class DimensionAccumulator[T](widths: Map[Int, Int] = Map(),
                                   heights: Map[Int, Int] = Map(),
                                   coordsToNode: Map[(Int, Int), T] = Map[(Int, Int), T]()) {
  lazy val maxRank = widths.keys.max
  lazy val maxLine = heights.keys.max

  def update(node: T, drawingContext: DrawingContext[T]): DimensionAccumulator[T] = {
    val bounds = drawingContext.sizeProvider.bounds(node)
    val rank = drawingContext.ranks(node)
    val line = drawingContext.lines(node)
    DimensionAccumulator[T](
      widths.updated(rank, widths.getOrElse(rank, 0).max(bounds.width)),
      heights.updated(line, heights.getOrElse(line, 0).max(bounds.height)),
      coordsToNode + ((rank, line) -> node))
  }

}

case class DrawingContext[T](graph: Graph[T],
                             ranks: Map[T, Int],
                             lines: Map[T, Int],
                             edgeMap: Map[Edge[T], Seq[Edge[T]]],
                             sizeProvider: SizeProvider[T]) {
  val dimensions = graph.nodes.foldLeft(DimensionAccumulator[T]()) { case (d, node) => d.update(node, this) }
  val (bounds, width, height) = makeNodeBounds()
  val edgeInfos = makeSideInfo()

  implicit def toEdgeOperation(edge: Edge[T]) = EdgeOperations(edge, lines)

  private def makeNodeBounds() = {
    val bounds = scala.collection.mutable.Map[T, Rectangle]()
    var y = sizeProvider.verticalMargin
    var width = 0
    var height = 0
    // TODO convert to fold
    0.to(dimensions.maxLine).foreach(line => {
      var x = sizeProvider.horizontalMargin
      0.to(dimensions.maxRank).foreach(rank => {
        // should be size 0 or 1
        dimensions.coordsToNode.get((rank, line)).foreach(node => {
          bounds += node -> Rectangle(Point(x, y), Size(dimensions.widths(rank), dimensions.heights(line)))
        })
        x += dimensions.widths(rank) + sizeProvider.horizontalMargin
        width = width.max(x)
      })
      y += dimensions.heights(line) + sizeProvider.verticalMargin
      height = height.max(y)
    })
    (bounds.toMap, width, height)
  }

  private def makeSideInfo() = {

    graph.nodes.map(node => {
      // add outgoing edge
      val outgoings = graph.outgoings(node).groupBy(edge => edge.getFromSide)
      // add incoming edge
      val incomings = graph.incomings(node).groupBy(edge => edge.getToSide)
      def compare(e1: Edge[T], e2: Edge[T], data: Map[T, Int]): Boolean = {
        data(e1.getOpposite(node)) < data(e2.getOpposite(node))
      }
      def sideInfo(side: Side.Side) = {
        val compareIncomings = (a: Edge[T], b: Edge[T]) => if (side == Side.LEFT) /*TODO check (b,a) */compare(a, b, ranks) else compare(a, b, lines)
        val compareOutgoings = (a: Edge[T], b: Edge[T]) => if (side == Side.RIGHT) compare(a, b, ranks) else compare(b, a, lines)
        val incomingEdges = incomings.get(side)
          .map(_.sortWith(compareIncomings))
          .getOrElse(Seq())
        val outgoingEdges = outgoings.get(side)
          .map(_.sortWith(compareOutgoings))
          .getOrElse(Seq())
        val edges = incomingEdges ++ outgoingEdges
        val straightEdge = edges.find(edge => lines(edge.from) == lines(edge.to))
        side -> SideEdgeInfo(node, side, edges, straightEdge)
      }
      node -> Map(
        sideInfo(Side.BOTTOM),
        sideInfo(Side.LEFT),
        sideInfo(Side.RIGHT))
    }).toMap
  }

  def getPoints(edge: Edge[T]): Seq[Point] = {
    val edges = edgeMap(edge)
    val firstPoints = makeEdgePoints(edges.head)
    if (edges.size > 1) {
        firstPoints.take(2) ++ makeEdgePoints(edges.last).drop(1)
    } else {
      firstPoints
    }
  }

  private def makeEdgePoints(edge: Edge[T]) = {
    val fromNode = edge.from
    val fromPoint = getAdjustedPoint(fromNode, edge)
    val toNode = edge.to
    val toPoint = getAdjustedPoint(toNode, edge)
    addCornerPoint(fromNode, toNode, fromPoint, toPoint) match {
      case Some(corner) =>
        Seq(fromPoint, corner, toPoint)
      case None =>
        Seq(fromPoint, toPoint)
    }
  }

  private def getAdjustedPoint(node: T, edge: Edge[T]): Point = {
    edgeInfos(node)(if (edge.from == node) edge.getFromSide else edge.getToSide).adjust(edge, bounds)
  }

  private def addCornerPoint(fromNode: T, toNode: T, fromPoint: Point, toPoint: Point) = {
    if (lines(fromNode) < lines(toNode)) {
      Some(Point(fromPoint.x, toPoint.y))
    } else if (lines(fromNode) > lines(toNode)) {
      Some(new Point(toPoint.x, fromPoint.y))
    } else {
      None
    }
  }
}
