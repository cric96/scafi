package it.unibo.scafi.simulation.gui.view.scalaFX.drawer

import javafx.scene
import javafx.scene.control.Label

import it.unibo.scafi.simulation.gui.launcher.SensorName._
import it.unibo.scafi.simulation.gui.model.core.World
import it.unibo.scafi.simulation.gui.model.graphics2D.BasicShape2D.{Circle => InternalCircle, Polygon => InternalPolygon, Rectangle => InternalRectangle}
import it.unibo.scafi.simulation.gui.model.simulation.implementation.mutable.SensorDefinition.{General, Led}
import it.unibo.scafi.simulation.gui.view.scalaFX._

import scalafx.application.Platform
import scalafx.geometry.{Point2D => FXPoint}
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Ellipse

object StandardFXDrawer extends FXDrawer {

  override type OUTPUTNODE = javafx.scene.Node
  val colors: Map[String, Color] = Map(sens1.name -> Color.Red,
    sens2.name -> Color.Yellow,
    sens3.name -> Color.Blue,
    gsensor.name -> Color.LimeGreen)
  val size: Map[String, Double] = Map(sens1.name -> 1, sens2.name -> 3, sens3.name -> 5, gsensor.name -> 7)
  val radius = 2

  override def nodeGraphicsNode(node: NODE): OUTPUTNODE = nodeToShape.create(node)

  override def deviceToGraphicsNode (node: OUTPUTNODE, dev: DEVICE): Option[OUTPUTNODE] = drawNode(dev,node)

  override def updateDevice(node : OUTPUTNODE, dev: DEVICE, graphicsDevice: Option[OUTPUTNODE]): Unit = {
    if(graphicsDevice.isEmpty) return
    val graphics = graphicsDevice.get
    dev match {
      case General(value) => {
        graphics.asInstanceOf[Label].setText(value.toString)
      }
      case Led(value) => {
        graphics.setVisible(value)
      }
      case _ =>
    }
  }

  private def drawNode (dev: DEVICE, node: OUTPUTNODE): Option[OUTPUTNODE] = {
    import scalafx.Includes._
    val point = nodeToAbsolutePosition(node)
    dev match {
      case General(value) => {
        val label = new Label(value.toString)
        label.layoutX.bind(node.translateX + point.x)
        label.layoutY.bind(node.translateY + point.y)
        Some(label)
      }
      case Led(value) => {
        val res : Node = new Ellipse {
          this.centerX.bind(node.translateX + point.x)
          this.centerY.bind(node.translateY + point.y)
          this.radiusX = size(dev.name.toString)
          this.radiusY = size(dev.name.toString)
          this.strokeWidth = radius
          val color: Color = colors(dev.name.toString)
          this.stroke = color
          this.fill = Color.Transparent
          this.visible = value
          this.smooth = false
        }
        Some(res)
      }
      case _ => None
    }
  }
}
