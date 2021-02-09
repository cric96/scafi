package it.unibo.scafi.js.utils

import scala.scalajs.js

object SpaceAdapter {
  class JSPoint3D(val x : Double, val y : Double, val z : Double) extends js.Object
  class JSPoint2D(val x : Double, val y : Double)

  object Zero2D extends JSPoint2D(0, 0)
  object Zero3D extends JSPoint2D(0, 0)
}
