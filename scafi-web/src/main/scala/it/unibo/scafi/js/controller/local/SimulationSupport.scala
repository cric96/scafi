package it.unibo.scafi.js.controller.local

import it.unibo.scafi.js.controller.AggregateSystemSupport
import it.unibo.scafi.js.controller.local.SimulationSideEffect._
import it.unibo.scafi.js.dsl.{BasicWebIncarnation, ScafiInterpreterJs}
import it.unibo.scafi.js.model._
import it.unibo.scafi.js.utils.SpaceAdapter.JSPoint2D
import it.unibo.scafi.simulation.SpatialSimulation
import it.unibo.scafi.space.Point3D
import monix.execution.Cancelable
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.ext.Color

import scala.concurrent.Future
import scala.scalajs.js
//TODO make support more general
/**
  * Support for manage a local aggregate simulation simulation
  * @param systemConfig initialize the backend with this configuration object
  */
class SimulationSupport(protected var systemConfig: SupportConfiguration)
                       (implicit val incarnation: BasicWebIncarnation, implicit val interpreter : ScafiInterpreterJs[BasicWebIncarnation])
  extends AggregateSystemSupport[SpatialSimulation#SpaceAwareSimulator, SupportConfiguration, SimulationSideEffect]
    with SideEffects {
  import SimulationSupport._
  import incarnation._
  protected var backend: SpaceAwareSimulator = fromConfig(systemConfig)

  protected val sideEffectsStream : PublishSubject[SimulationSideEffect] = PublishSubject()

  import monix.execution.Scheduler.Implicits.global

  override val graphStream: Observable[Graph] = sideEffectsStream.scan(Graph.empty)(mapSideEffect).share
  private val localSubscriber : Cancelable = graphStream.subscribe() // turn graph stream in an hot one
  sideEffectsStream.onNext(Invalidated)

  override def evolve(config: SupportConfiguration): Future[Unit] = Future.successful{
    fromConfig(config)
    invalidate()
  }

  def invalidate() : Unit = sideEffectsStream.onNext(Invalidated)

  private def fromConfig(config: SupportConfiguration) : SpaceAwareSimulator = {
    backend = (config.network, config.neighbour) match {
      case (grid : GridLikeNetwork, SpatialRadius(range)) =>
        simulatorFactory.gridLike(grid.toGridSettings, range, seeds = backendSeed(config)).asInstanceOf[SpaceAwareSimulator]
      case (random : RandomNetwork, SpatialRadius(range)) =>
        //TODO FIX
        simulatorFactory.random(random.min, random.max, range, random.howMany, backendSeed(config)).asInstanceOf[SpaceAwareSimulator] //todo improve this
      case _ => throw new IllegalArgumentException("configuration not supported")
    }
    config.deviceShape.sensors.foreach { case (sensorName, value) => backend.addSensor(sensorName, value) }
    for ((id, sensorValues) <- config.deviceShape.initialValues) {
      for ((sensorName, sensorValue) <- sensorValues) {
        backend.chgSensorValue(sensorName, Set(id), sensorValue)
      }
    }
    systemConfig = config
    backend
  }

  private def mapSideEffect(graph : Graph, sideEffect : SimulationSideEffect) : Graph = {
    (sideEffect, graph) match {
      case (NewConfiguration, _) => produceGraphFromNetwork()
      case (Invalidated, _) => produceGraphFromNetwork()
      case (ExportProduced(elements), graph) => updateGraphWithExports(elements, graph)
      case (PositionChanged(positionMap), graph) => updateGraphWithPosition(positionMap, graph)
      case (SensorChanged(sensorMap), graph) => updateGraphWithSensor(sensorMap, graph)
      case _ => produceGraphFromNetwork()
    }
  }

  private def produceGraphFromNetwork() : Graph = {
    val nodes : Seq[Node] = backend.exports()
      .map { case (id, export) => (backend.devs(id), export )}
      .map {
        case (dev, Some(export)) => (dev, dev.lsns + (EXPORT_LABEL -> export))
        case (dev, None) => (dev, dev.lsns)
      }
      .map { case (dev, labels) => new Node(dev.id, new JSPoint2D(dev.pos.x, dev.pos.y), mapToDictionary(labels)) }
      .toSeq
    val vertices = computeVertices()
    new NaiveGraph(js.Array(nodes:_*), vertices)
  }

  import GraphOps.Implicits._
  private def updateGraphWithExports(exports: Seq[(ID, EXPORT)], graph: Graph) : Graph = {
    val newExports = exports.map { case (id, export) => export -> graph(id) }
      .map { case (export, node) => new Node(node.id, node.position, node.labels ++: js.Dictionary(EXPORT_LABEL -> (export).asInstanceOf[js.Any])) }
    graph.insertNodes(newExports)
  }

  private def updateGraphWithPosition(positionMap : Map[ID, Point3D], graph : Graph) : Graph = {
    val nodesUpdated = positionMap.map { case (id, pos) => pos -> graph(id) }
      .map { case (pos, node) => new Node(node.id, new JSPoint2D(pos.x, pos.y), node.labels) }
      .toSeq
    new NaiveGraph(graph.insertNodes(nodesUpdated).nodes, computeVertices()) //neighbour could be change, todo improve performance
  }

  private def updateGraphWithSensor(sensorMap : Map[ID, Map[LSNS, Any]], graph : Graph) = {
    val nodeUpdated = sensorMap.toSeq.map { case (id, labels) => labels -> graph(id) }
      .map { case (labels, node) => new Node(node.id, node.position, node.labels ++: mapToDictionary(labels)) }
    graph.insertNodes(nodeUpdated)
  }

  private def computeVertices() : js.Array[Vertex] = js.Array(backend.getAllNeighbours()
    .flatMap { case (id, elements) => elements.map(new Vertex(id, _)) }
    .toSeq:_*
  )

  private def backendSeed(config : SupportConfiguration) : Seeds = {
    val SimulationSeeds(configSeed, simulationSeed, randomSensorSeed) = config.seed
    Seeds(configSeed.toLong, simulationSeed.toLong, randomSensorSeed.toLong)
  }

  private def mapToDictionary(data : Map[LSNS, Any]) : js.Dictionary[js.Any] = {
    js.Dictionary(data.mapValues(_.asInstanceOf[js.Any]).toSeq:_*)
  }
}

object SimulationSupport {
  val EXPORT_LABEL = "export"
}
