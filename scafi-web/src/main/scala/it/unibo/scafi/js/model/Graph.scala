package it.unibo.scafi.js.model

import it.unibo.scafi.js.utils.SpaceAdapter.JSPoint2D
import it.unibo.scafi.space.Point3D

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
//TODO add enviroment concept? What is it? It is Any? has some constraint?
//TODO modify to became fully javascript complaint (using js.Array and js.Dictionary)
/**
 * A graph model used to represent an aggregate system.
 * Conceptually this data structure should be immutable.
 * This graph contains a set of nodes uniquely identified by an id. Each node has a set
 * of labels that decorate a node with some useful information (e.g. sensor value,...).
 * The graph has also a set of vertex that link nodes with each other.
 */
trait Graph extends js.Object {
  def contains(id : String) : Boolean
  @JSName("take")
  def apply(id : String) : Node
  def get(id : String) : js.UndefOr[Node]
  def nodes : js.Array[Node]
  def vertices : js.Array[Vertex]
  def neighbours(id : String) : js.Array[Node]
  def neighbours(node : Node) : js.Array[Node]
}

object Graph {
  implicit def tupleToVertex(tuple : (String, String)) : Vertex = new Vertex(tuple._1, tuple._2)
  def empty : Graph = new NaiveGraph(js.Array(), js.Array())
}
class Vertex(val from : String, val to : String) extends js.Object
//position is a first class element or could be ignored?
class Node(val id : String, val position : JSPoint2D, val labels : js.Dictionary[js.Any] = js.Dictionary()) extends js.Object {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString : String = s"Node($id, $position, $labels)"
}
