package com.github.madbrain.graphlayout

case class EdgeOperations[T](edge: Edge[T], lines: Map[T, Int]) {
  def getFromSide: Side.Side = if (lines(edge.from) < lines(edge.to)) {
    Side.BOTTOM
  } else {
    Side.RIGHT
  }

  def getToSide: Side.Side = if (lines(edge.from) > lines(edge.to)) {
    Side.BOTTOM
  } else {
    Side.LEFT
  }

  def getOpposite(node: T) = if (node == edge.from) edge.to else edge.from
}