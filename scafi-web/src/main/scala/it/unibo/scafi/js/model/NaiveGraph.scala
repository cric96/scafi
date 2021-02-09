package it.unibo.scafi.js.model

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

//TODO think if it is necessary to throws or not an exception
class NaiveGraph(val nodes : js.Array[Node], val vertices : js.Array[Vertex]) extends Graph {
  private lazy val internalMap = js.Dictionary(nodes.map(node => node.id -> node): _*)
  //require(vertices.forall(vertex => contains(vertex.from) && contains(vertex.to))) //TODO this safety check is very slow, think how to manage this
  private lazy val neighbourMap = vertices
    .map(vertex => vertex.from -> internalMap(vertex.to))
    .groupBy(_._1)
    .mapValues(value => value.map(_._2))
  override def contains(id: String): Boolean = internalMap.contains(id)
  @JSName("take")
  override def apply(id: String): Node = internalMap(id)

  override def get(id: String): js.UndefOr[Node] = if(internalMap.contains(id)) {
    internalMap(id)
  } else {
    js.undefined
  }

  override def neighbours(id: String): js.Array[Node] = neighbourMap.getOrElse(id, js.Array())

  override def neighbours(node: Node): js.Array[Node] = neighbourMap.getOrElse(node.id, js.Array())
}
