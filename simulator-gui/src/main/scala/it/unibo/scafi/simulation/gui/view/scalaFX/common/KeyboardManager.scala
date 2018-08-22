package it.unibo.scafi.simulation.gui.view.scalaFX.common

import javafx.scene.input.KeyCombination

import it.unibo.scafi.simulation.gui.configuration.command.CommandFactory.CommandArg
import it.unibo.scafi.simulation.gui.controller.input.InputCommandController
import it.unibo.scafi.simulation.gui.util.Result.Success
import it.unibo.scafi.simulation.gui.view.AbstractKeyboardManager
import it.unibo.scafi.simulation.gui.view.AbstractKeyboardManager._

import scalafx.scene.input.{KeyCode, KeyCodeCombination, KeyEvent}
trait KeyboardManager extends AbstractKeyboardManager {

  self: AbstractFXSimulationPane with FXSelectionArea =>
  override type KEY_CODE = KeyCode
  override type KEYCODE_COMBINATION = KeyCodeCombination

  abstractToReal += Code1 -> KeyCode.Digit1
  abstractToReal += Code2 -> KeyCode.Digit2
  abstractToReal += Code3 -> KeyCode.Digit3
  abstractToReal += Code4 -> KeyCode.Digit4
  abstractToReal += Code5 -> KeyCode.Digit5

  abstractToCombination += Undo -> new KeyCodeCombination(KeyCode.Z, KeyCombination.CONTROL_DOWN)
  import scalafx.Includes._

  self.onKeyPressed = (e: KeyEvent) => {
    e.consume()
    this.abstractToReal.filter{x => x._2 == e.getCode()}.foreach{x => computeCommand(x._1)}
    this.abstractToCombination.filter { x => x._2.`match`(e)}.foreach(x => computeCommand(x._1))
  }

  private def computeCommand(code : AbstractKeyCode): Unit = {
    this.commandArgs.get(code) match {
      case Some(standardArg : CommandArg) => {
        val arg : CommandArg = if(this.valueMapped.get(code).isDefined) {
          (standardArg + (this.valueMapped(code) -> selected))
        } else {
          standardArg
        }
        InputCommandController.virtualMachine.process((factoryMapped(code), arg))
      }
    }

  }
}

