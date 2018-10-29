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

package frontend.sims

import it.unibo.scafi.simulation.frontend.incarnation.scafi.bridge.ScafiSimulationInitializer.RadiusSimulation
import it.unibo.scafi.simulation.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.frontend.incarnation.scafi.configuration.{ScafiProgramBuilder, ScafiWorldInformation}
import it.unibo.scafi.simulation.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.frontend.launcher.scafi.{ListDemo, Console => ScafiConsole, GraphicsLauncher => ScafiGraphicsLauncher, StringLauncher => ScafiStringLauncher}
import it.unibo.scafi.simulation.frontend.view.scalaFX.drawer.FastFXOutput
import it.unibo.scafi.space.graphics2D.BasicShape2D.Circle


object DemoLauncher extends App {
  ScafiProgramBuilder (
    Random(5000,1920,1080),
    SimulationInfo(program = classOf[Main]),
    RadiusSimulation(radius = 40),
    neighbourRender = true,
    scafiWorldInfo = ScafiWorldInformation(shape = Some(Circle(4))),
    outputPolicy = FastFXOutput
  ).launch()
}

object ConsoleLauncher extends App {
  ListDemo.packageName = "frontend.sims"
  ScafiConsole.main(args)
}

object GraphicsLauncher extends App {
  ListDemo.packageName = "frontend.sims"
  ScafiGraphicsLauncher.main(args)
}

object StringLauncher extends App {
  ListDemo.packageName = "frontend.sims"
  ScafiStringLauncher("radius-simulation SupplyRescueDemo 50;random-world 500 1000 1000")
}
