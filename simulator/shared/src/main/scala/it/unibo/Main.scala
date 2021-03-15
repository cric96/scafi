package it.unibo

import it.unibo.scafi.config.GridSettings
import it.unibo.scafi.incarnations.BasicAbstractSpatialSimulationIncarnation
import it.unibo.scafi.lib.StandardLibrary
import it.unibo.scafi.space.Point3D

import java.time.Instant
//A mini bench for aggregate simulation execution
object Main {
  object MyIncarnation extends BasicAbstractSpatialSimulationIncarnation with StandardLibrary {
    override implicit val idBounded: Main.MyIncarnation.Builtins.Bounded[Int] = Builtins.Bounded.of_i
    override type P = Point3D
  }
  val range = 90
  val step = 60
  val runLength = 1000
  val sourceId : Int = 0
  def main(args: Array[String]): Unit = {
    import MyIncarnation._

    val simulation = MyIncarnation.simulatorFactory.gridLike(GridSettings(stepx = step, stepy = step), rng = range)
    val program = new AggregateProgram {
      override def main(): Any = rep(0)(_ + 1)
    }

    val gradient : AggregateProgram = new AggregateProgram with StandardSensors with BlockG {
      override def main(): Any = classicGradient(mid() == sourceId)
    }

    val gradientTime = evalTime { repeat(runLength)(simulation.exec(gradient))}
    simulation.clearExports()
    val roundTime = evalTime { repeat(runLength)(simulation.exec(program)) }
    println(s"round counter time of $runLength execution = $roundTime")
    println(s"gradient time of $runLength execution = $gradientTime")
  }

  private def repeat(times : Int)(fun : => Any) : Unit = (0 to times) foreach { _ => fun }
  private def evalTime(any : => Unit) : Long = {
    val initial = Instant.now()
    any
    Instant.now().minusMillis(initial.toEpochMilli).toEpochMilli
  }
}
