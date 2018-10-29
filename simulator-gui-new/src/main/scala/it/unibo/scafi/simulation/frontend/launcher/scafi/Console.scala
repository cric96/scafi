package it.unibo.scafi.simulation.frontend.launcher.scafi

import it.unibo.scafi.simulation.frontend.configuration.parser.{ConfigurationMachine, RuntimeMachine}
import it.unibo.scafi.simulation.frontend.controller.logger.LogManager

/**
  * a scafi program launcher via console with unix like language
  * to run a simulation in console you can type:
  * <pre>
  *   {@code
  *      random-world 1000 500 500
  *      radius-simulation Simple 10
  *      launch
  *   }
  * </pre>
  */
object Console extends App {
  import it.unibo.scafi.simulation.frontend.configuration.launguage.ResourceBundleManager._
  import it.unibo.scafi.simulation.frontend.controller.logger.LogManager._
  import it.unibo.scafi.simulation.frontend.incarnation.scafi.configuration.ScafiInformation._
  val configurationMachine = new ConfigurationMachine(UnixConfiguration)
  val runtimeMachine = new RuntimeMachine(UnixRuntime)
  println(international("welcome")(KeyFile.Configuration))
  val log = new ConsoleOutputObserver
  LogManager.attach(log)
  while(!configurationBuilder.created){
    LogManager.notify(StringLog(Channel.CommandResult,Label.Empty,configurationMachine.process(readLine())))
  }
  LogManager.detach(log)
  while(true) {
    println(runtimeMachine.process(readLine()))
  }
}
