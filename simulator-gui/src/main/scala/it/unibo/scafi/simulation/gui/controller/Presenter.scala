package it.unibo.scafi.simulation.gui.controller

import it.unibo.scafi.simulation.gui.model.aggregate.AggregateWorld
import it.unibo.scafi.simulation.gui.view.Output

/**
  * controls the world and update the output
  */
trait Presenter[W <: AggregateWorld] extends Controller[W]{
  type OUTPUT <: Output
}