package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout.{Segment, Rectangle}
import org.scalatest.matchers.{MatchResult, Matcher}

trait GeometryMatchers {

  class RectangleOverlapMatcher(right: Rectangle) extends Matcher[Rectangle] {

    def apply(left: Rectangle) = {
      MatchResult(
        left.overlaps(right),
        s"""$left and $right don't overlap"""",
        s"""$left and $right overlap""""
      )
    }
  }

  def overlap(other: Rectangle) = new RectangleOverlapMatcher(other)

  class SegmentOverlapMatcher(right: Segment) extends Matcher[Rectangle] {

    def apply(left: Rectangle) = {
      val overlap = if (right.isVertical) {
        val min = right.min.y
        val max = right.max.y
        left.x < right.from.x && right.from.x < left.getMaxX && left.getMaxY > min && left.y < max
      } else {
        val min = right.min.x
        val max = right.max.x
        left.y < right.from.y && right.from.y < left.getMaxY && left.getMaxX > min && left.x < max
      }
      MatchResult(
        overlap,
        s"""$left and $right don't overlap"""",
        s"""$left and $right overlap""""
      )
    }
  }

  def overlap(other: Segment) = new SegmentOverlapMatcher(other)
}

