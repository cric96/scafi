package it.unibo.scafi.js.controller.local

import it.unibo.scafi.js.controller.ExecutionPlatform
import it.unibo.scafi.js.controller.local.SimulationExecution.TickBased
import it.unibo.scafi.js.controller.local.SimulationSideEffect.SideEffects
import it.unibo.scafi.js.controller.scripting.Script
import it.unibo.scafi.js.controller.scripting.Script.{Javascript, ScaFi, Scala}
import it.unibo.scafi.js.dsl.JF1
import it.unibo.scafi.simulation.SpatialSimulation

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.util.{Failure, Success, Try}
import org.scalajs.dom
import dom.ext.{Ajax, AjaxException}
import org.scalajs.dom.document

/**
  * the execution platform of a local simulation in web browser.
  * Currently it supports only javascript execution.
  */
trait SimulationExecutionPlatform extends ExecutionPlatform[SpatialSimulation#SpaceAwareSimulator, SimulationSideEffect, SimulationExecution]{
  self : SimulationSupport with SideEffects =>
  import incarnation._
  //TODO add better support
  import scala.concurrent
    .ExecutionContext
    .Implicits
    .global

  val location = document.location
  val server = s"${location.protocol}//${location.host}"
  val url = s"$server/code"

  override def loadScript(script: Script): Future[SimulationExecution] = script match {
    case Javascript(code) => Future.fromTry {
      Try { interpreter.adaptForScafi(code) }
        .map(_.asInstanceOf[JF1[CONTEXT, EXPORT]]) //TODO NOT SAFE! FIND ANOTHER WAY
        .map(sideEffectExecution)
    }
    case aggregateClass : ScaFi[AggregateProgram] => Future.fromTry {
      Try { sideEffectExecution(aggregateClass.program) }
    }
    case Scala(code) =>
      val result = Ajax.post(url, Ajax.InputData.str2ajax(code))
      result
        .filter(_.status == 200)
        .map(_.responseText)
        .map{ id =>
          document.body.innerHTML = "" //TODO NOT SAFE
          val newScript = document.createElement("script").asInstanceOf[dom.html.Script]
          newScript.src = s"$server/js/$id"
          newScript.`type` = "text/javascript"
          newScript.id = "scafiWeb"
          document.body.appendChild(newScript)
          //document.location.replace(s"$server/compilation/$id") //injection...
          sideEffectExecution(new AggregateProgram {
            override def main(): Any = {}
          }) //TODO USELESS, find another way
        }
    case _ => Future.failed(new IllegalArgumentException("lang not supported"))
  }
  private def sideEffectExecution(program : js.Function1[CONTEXT, EXPORT]) : TickBased = {
    val execution : (Int => Future[Unit]) = batchSize => {
      val execution = Future.fromTry(Try[Unit] {
        val exports = (0 until batchSize).map(_ => backend.exec(program))
        sideEffectsStream.onNext(ExportProduced(exports))
      })
      execution
    }
    backend.clearExports() //clear export for the new script
    sideEffectsStream.onNext(Invalidated) //invalid old graph value
    TickBased(exec = execution)
  }
}