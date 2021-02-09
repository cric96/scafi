package it.unibo.scafi.js.model

import scala.scalajs.js

/**
 * define a set of operations used to alter nodes inside a graph
 */
trait NodeOperation {
  /**
   * Remove the node selected from the graph. If the node isn't in the graph, return the
   * current graph
   */
  def removeNode(node : String) : Graph

  /**
   * Insert the node in the graph. If the node is already in the graph, this method return a new
   * version of graph with the new node.
   */
  def insertNode(node : Node) : Graph
}

/**
  * define a set of operations used to alters a set of node inside a graph
  */
trait BulkNodeOperation {
  /**
    * Remove nodes selected from the graph (if they are present).
    */
  def removeNodes(node : Seq[String]) : Graph

  /**
    * Insert nodes in the graph. If the node is already in the graph, this method update the nodes
    */
  def insertNodes(node : Seq[Node]) : Graph
}
/**
 * define a set of operations used to alter vertex inside a graph
 */
trait VertexOperation {
  /**
   * Remove the selected vertex from the graph. If it isn't in the graph, return current the current
   * version of the graph.
   */
  def unlink(vertex : Vertex) : Graph

  /**
   * Insert the vertex in the graph.
   */
  def link(vertex: Vertex) : Graph
}
trait NeighbourOperation {
  /**
   * alter the entirely neighbours of a node
   */
  def replaceNeighbours(node : String, neighbour : Set[String]) : Graph
}
object GraphOps {
  object Implicits {
    implicit class RichGraph(graph: Graph) extends NodeOperation with VertexOperation with NeighbourOperation with BulkNodeOperation {
      override def removeNode(node: String): Graph = graph match {
        case g : NodeOperation => g.removeNode(node)
        case _ =>
          val nodes = graph.nodes.filterNot(_.id == node) //remove node, equals is on id, not on point.
          val vertices = graph.vertices.filterNot(vertex => vertex.to == node || vertex.from == node) //remove vertex in witch node is partecipant
          new NaiveGraph(nodes, vertices)
      }

      override def insertNode(node: Node): Graph = graph match {
        case g : NodeOperation => g.insertNode(node)
        case _ => new NaiveGraph(graph.nodes.filterNot(_.id == node.id) ++ js.Array(node), graph.vertices) //first remove old occurrence and then insert new one
      }

      override def unlink(vertex: Vertex): Graph = graph match {
        case g : VertexOperation => g.link(vertex)
        case _ => new NaiveGraph(graph.nodes, graph.vertices.filterNot(v => v.to == vertex.to && v.from == vertex.from))
      }
      override def link(vertex: Vertex): Graph = graph match {
        case g : VertexOperation => g.unlink(vertex)
        case _ => new NaiveGraph(graph.nodes, graph.vertices ++ js.Array(vertex))
      }

      override def replaceNeighbours(node: String, neighbours: Set[String]): Graph = graph match {
        case g : NeighbourOperation => g.replaceNeighbours(node, neighbours)
        case _ =>
          val oldNeighbours = graph.neighbours(node)
          val vertex = graph.vertices
          val toRemove = oldNeighbours.map(neighbour => new Vertex(node, neighbour.id))
          val toAdd = neighbours.map(neighbour => new Vertex(node, neighbour))
          val removedVertices = vertex.filterNot(vertex => toRemove.exists(v => v.from == vertex.from && vertex.to == v.to))
          new NaiveGraph(graph.nodes, removedVertices ++ toAdd)
      }

      override def removeNodes(nodes: Seq[String]): Graph = graph match {
        case g : BulkNodeOperation => g.removeNodes(nodes)
        case _ => val nodesToRemove = nodes map { graph.get } map { _.toOption } collect { case Some(n) => n }
          val newVertices = graph.vertices filter { vertex => nodes.contains(vertex.from) || nodes.contains(vertex.to) }
          val newNodes = graph.nodes.filterNot(node => nodesToRemove.exists(_.id == node.id))
          new NaiveGraph(newNodes, newVertices)
      }

      override def insertNodes(nodes: Seq[Node]): Graph = graph match {
        case g : BulkNodeOperation => g.insertNodes(nodes)
        case _ =>
          val removeOld = (graph.nodes.filterNot(node => nodes.exists(_.id == node.id)))
          new NaiveGraph(removeOld++ nodes, graph.vertices)
      }
    }
  }
}
