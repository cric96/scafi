/*
 * Copyright (C) 2016-2017, Roberto Casadei, Mirko Viroli, and contributors.
 * See the LICENCE.txt file distributed with this work for additional
 * information regarding copyright ownership.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
*/

package sims

import it.unibo.scafi.incarnations.BasicSimulationIncarnation.{AggregateProgram, BlockG, Builtins}
import it.unibo.scafi.simulation.gui.incarnation.scafi.bridge.ScafiSimulationInitializer.RadiusSimulation
import it.unibo.scafi.simulation.gui.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.gui.incarnation.scafi.bridge.reflection.Demo
import it.unibo.scafi.simulation.gui.incarnation.scafi.configuration.ScafiProgramBuilder
import it.unibo.scafi.simulation.gui.incarnation.scafi.world.ScafiWorldInitializer.{Grid, Random}
import it.unibo.scafi.simulation.gui.view.scalaFX.drawer.{FastFXOutput, StandardFXOutput}

object ChannelDemo extends App {
  ScafiProgramBuilder (
    Random(1000,500,500),
    SimulationInfo(program = classOf[Channel]),
    RadiusSimulation(radius = 20),
    outputPolicy = StandardFXOutput,
    neighbourRender = true
  ).launch()
}

/**
  * Channel with obstacles
  *   - Sense1: source area
  *   - Sense2: destination area
  *   - Sense3: obstacles
  */
@Demo
class Channel extends AggregateProgram  with SensorDefinitions with BlockG {

  def channel(source: Boolean, target: Boolean, width: Double): Boolean =
    distanceTo(source) + distanceTo(target) <= distanceBetween(source, target) + width

  override def main() = branch(sense3){false}{channel(sense1, sense2, 1)}
}
@Demo
class SelfContainedChannel extends AggregateProgram with SensorDefinitions {
  override def main() = branch(sense3){false}{channel(sense1, sense2, 5)}

  type OB[T] = Builtins.Bounded[T]
  def G[V:OB](src: Boolean, field: V, acc: V=>V, metric: =>Double): V =
    rep( (Double.MaxValue, field) ){ dv =>
      mux(src) { (0.0, field) } {
        minHoodPlus {
          val (d, v) = nbr { (dv._1, dv._2) }
          (d + metric, acc(v))
        } } }._2

  def gradient(source: Boolean): Double =
    G[Double](source, 0, _ + nbrRange(), nbrRange())

  def broadcast[V:OB](source: Boolean, field: V): V =
    G[V](source, field, x=>x, nbrRange())

  def distBetween(source: Boolean, target: Boolean): Double =
    broadcast(source, gradient(target))

  def dilate(region: Boolean, width: Double) =
    gradient(region) < width

  def channel(src: Boolean, dest: Boolean, width: Double) =
    dilate(gradient(src) + gradient(dest) <= distBetween(src,dest), width)
}
