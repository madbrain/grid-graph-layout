package com.github.madbrain.graphlayout

trait Edge[T] {
  val from: T
  val to: T
}

case class NormalEdge[T](from: T, to: T) extends Edge[T]

case class ReversedEdge[T](from: T, to: T, original: Edge[T]) extends Edge[T]

case class Graph[T](nodes: Seq[T], edges: Seq[Edge[T]]) {
  lazy val incomings = nodes.map(n => n -> edges.filter(e => e.to == n)).toMap
  lazy val outgoings = nodes.map(n => n -> edges.filter(e => e.from == n)).toMap
  def sources = nodes.filter(n => incomings(n).isEmpty)
}

trait NodeHelper[T] {
  def isDummy(node: T): Boolean
  def createDummy(nodes: Set[T]): T
}