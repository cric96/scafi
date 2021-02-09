package it.unibo.scafi.js.model

import it.unibo.scafi.js.utils.SpaceAdapter.Zero2D
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.scalajs.js

class NaiveGraphTest extends AnyFunSpec with Matchers {
  import NaiveGraphTest._

  describe("Naive graph") {
    it("has correct nodes and vertex") {
      val nodes = js.Array(node("1"), node("2"), node("3"))
      val vertices = js.Array(new Vertex("1", "2"))
      val graph = new NaiveGraph(nodes, vertices)
      nodes shouldBe graph.nodes
      vertices shouldBe graph.vertices
    }
    it("has correct neighbours") {
      val nodes = js.Array(node("1"), node("2"), node("3"))
      val vertices = js.Array(new Vertex("1", "2"), new Vertex("1", "3"))
      val graph = new NaiveGraph(nodes, vertices)
      val neighboursId = graph.neighbours("1").map(_.id)
      neighboursId.toSeq shouldBe js.Array("2", "3").toSeq
      //it is directed:
      graph.neighbours("2").isEmpty shouldBe true
      graph.neighbours("2").toSeq shouldBe graph.neighbours(node("2")).toSeq
    }
    /*TODO if there are serious performance problems, this check must be cancelled
    it("should throw exception if vertex set contains a node not present in the graph") {
      val nodes = Set(node("1"))
      val vertices = Set(Vertex("1", "2"))
      assertThrows[IllegalArgumentException] {
        NaiveGraph(nodes, vertices)
      }
    }*/
    it("apply works as expected ") {
      standardGraph("1").id shouldBe node("1").id
      standardGraph("1").labels.toSeq shouldBe node("1").labels.toSeq

    }
    it("apply throws exception if the id isn't present") {
      assertThrows[NoSuchElementException]{standardGraph("bibo")}
    }
    it("get return node if the node is present") {
      standardGraph.get("1").toOption.map(_.id) shouldBe Some(node("1").id)
    }
    it("get return None if the node isn't present") {
      standardGraph.get("bibo").toOption shouldBe None
    }
  }
  import Graph._
  import GraphOps.Implicits._ //graphs implicit
  describe("Naive graphs ops") {
    it("don't alter graph") {
      standardGraph.insertNode(node("3"))
      standardGraph contains("3") shouldBe false
    }

    it("insert node return graph with new node") {
      val newGraph = standardGraph.insertNode(node("3"))
      newGraph contains("3") shouldBe true
      newGraph shouldNot be(standardGraph)
    }

    it("insert node update existing node") {
      val label = js.Dictionary("label" -> { 10 : js.Any} )
      val newNode = new Node("1", Zero2D, label)
      val newGraph = standardGraph.insertNode(newNode)
      newGraph("1").labels shouldBe label
    }

    it("remove node return graph without the node node") {
      val newGraph = standardGraph.removeNode("1")
      newGraph contains("1") shouldBe false
    }

    it("remove node not present return the same graph") {
      val newGraph = standardGraph.removeNode("lemmy")
      newGraph.nodes.toSeq shouldBe standardGraph.nodes.toSeq
    }

    it("link add vertex between two nodes") {
      val newVertex : Vertex = "2" -> "1"
      val newGraph = standardGraph.link(newVertex)
      newGraph.vertices contains newVertex shouldBe true
      newGraph.vertices shouldNot be(standardGraph.vertices)
    }
    /*TODO to performance reason this test can be removed
    it("link a non existing node throws exception") {
      val newVertex : Vertex = "3" -> "1"
      assertThrows[IllegalArgumentException](standardGraph.link(newVertex))
    }*/

    it("unlink a vertex return an updated graph without that link") {
      val vertexToRemove : Vertex = "1" -> "2"
      val newGraph = standardGraph.unlink(vertexToRemove)
      newGraph.vertices contains vertexToRemove shouldBe false
      newGraph.neighbours("1").toSeq shouldBe Seq()
    }

    it("allow to change enterly a neighbour of a node") {
      val nodes = js.Array(node("1"), node("2"), node("3"), node("4"))
      val vertex = js.Array(new Vertex("1", "2"))
      val graph = new NaiveGraph(nodes, vertex)
      val newGraph = graph.replaceNeighbours("1", Set("1", "3", "4"))
      newGraph.neighbours("1").map(_.id).toSeq shouldBe Seq("1", "3", "4")
      val newNeighbours = Set(("1", "1"), ("1", "3"), ("1", "4"))
      val safeVertices = newGraph.vertices.map(v => (v.from, v.to)).toSet
      safeVertices shouldBe newNeighbours
    }
  }
}

object NaiveGraphTest {
  def node(id : String) : Node = new Node(id, Zero2D)
  val standardGraph = new NaiveGraph(js.Array(node("1"), node("2")), js.Array(new Vertex("1", "2")))
}
