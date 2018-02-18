package com.github.madbrain.graphlayout.test

import com.github.madbrain.graphlayout.{Edge, Graph, NormalEdge}
import org.scalacheck.Gen

object GraphBuilder {

  def makeEdges(i: Int) = {
    val n = i.toString
    for {
      max <- Gen.choose(1, 3)
      others <- Gen.pick(max min i, 0.until(i))
      seq <- Gen.sequence[Seq[Edge[String]], Edge[String]](others.map(other => {
        Gen.oneOf(NormalEdge(other.toString, n), NormalEdge(n, other.toString))
      }))
    } yield {
     seq
    }
  }

  def genEdgesForNodes(n: Int) = {
    Gen.sequence[Seq[Seq[Edge[String]]], Seq[Edge[String]]](
      1.to(n).map(i => makeEdges(i)))
  }

  val genGraph: Gen[Graph[String]] = for {
    n <- Gen.choose(10, 20)
    edges <- genEdgesForNodes(n)
  } yield {
    Graph(0.to(n).map(_.toString), edges.flatten)
  }
}
