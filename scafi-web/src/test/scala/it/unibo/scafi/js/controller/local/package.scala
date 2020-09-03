package it.unibo.scafi.js.controller

package object local {
  val standardConfig = {
    val (cols, rows, stepX, stepY, tolerance) = (10, 10, 20, 20, 0)
    SupportConfiguration(
      GridLikeNetwork(cols, rows, stepX, stepY, tolerance),
      SpatialRadius(stepX),
      DeviceConfiguration.none,
      SimulationSeeds()
    )
  }
}