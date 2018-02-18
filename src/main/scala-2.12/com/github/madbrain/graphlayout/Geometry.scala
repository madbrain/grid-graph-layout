
package com.github.madbrain.graphlayout

case class Point(x: Int, y: Int)

case class Rectangle(origin: Point, size: Size) {
  def overlaps(other: Rectangle): Boolean = {
    x <= other.getMaxX && other.x <= getMaxX && y >= other.getMaxY && other.y >= getMaxY
  }

  def x = origin.x

  def y = origin.y

  def width = size.width

  def height = size.height

  def getCenterX = origin.x + size.width / 2

  def getCenterY = origin.y + size.height / 2

  def getMaxX = origin.x + size.width

  def getMaxY = origin.y + size.height
}

case class Segment(from: Point, to: Point) {
  def isVertical = from.x == to.x
  def min = Point(from.x.min(to.x), from.y.min(to.y))
  def max = Point(from.x.max(to.x), from.y.max(to.y))
}

case class Size(width: Int, height: Int)

object Side {

  sealed trait Side

  case object LEFT extends Side

  case object RIGHT extends Side

  case object TOP extends Side

  case object BOTTOM extends Side

}