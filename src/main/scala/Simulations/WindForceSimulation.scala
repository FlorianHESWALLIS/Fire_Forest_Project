package Simulations

import fire.model.{Grid, Meteo, Simulation}
import fire.model.CellState.Burned

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.Locale

object WindForceSimulation {

  val gridSizeX = 40
  val gridSizeY = 140
  val nSteps = 11000
  val begin = 0.0
  val end = 30.0
  val step = 0.1

  // Les valeurs de mW à tester
  val mWValues: Seq[Double] = (BigDecimal(0.00) to BigDecimal(0.10) by BigDecimal(0.01)).map(_.toDouble)

  val windSpeeds: Seq[Double] =
    (BigDecimal(begin) to BigDecimal(end) by BigDecimal(step)).map(_.toDouble)

  // Grille initiale avec mW imposé
  def initialGrid(mW: Double): Grid = {
    val g = Grid.initializeGrid(gridSizeX, gridSizeY)

    val updatedCells = g.cells.map { row =>
      row.map { cell =>
        cell.copy(mW = mW)
      }
    }

    val fireI = gridSizeX / 3
    val fireJ = gridSizeY / 2
    val burningCell = updatedCells(fireI)(fireJ).copy(state = fire.model.CellState.Burning, T = 1850.0)
    val updatedRow = updatedCells(fireI).updated(fireJ, burningCell)
    val finalCells = updatedCells.updated(fireI, updatedRow)

    g.copy(cells = finalCells)
  }

  def burnedRatio(grid: Grid): Double = {
    val total = grid.size_X * grid.size_Y
    val burned = grid.cells.flatten.count(_.state == Burned)
    burned.toDouble / total
  }

  def progressBar(progress: Double, width: Int = 40): String = {
    val filled = (progress * width).toInt
    val bar = "=" * filled + " " * (width - filled)
    f"[$bar] ${progress * 100}%.1f%%"
  }

  def runSimFor(mW: Double, v: Double): (Double, Double, Double) = {
    val meteo = Meteo((0, -1), v, 1.0)
    val sim = new Simulation(initialGrid(mW), meteo)
    val lastGrid = sim.allSteps().drop(nSteps).headOption.getOrElse(sim.allSteps().last)
    val ratio = burnedRatio(lastGrid)
    (mW, v, ratio)
  }

  def waitWithProgress(fs: Seq[Future[(Double, Double, Double)]]): Seq[(Double, Double, Double)] = {
    val total = fs.size
    var done = 0
    while (done < total) {
      done = fs.count(_.isCompleted)
      print("\r" + progressBar(done.toDouble / total))
      Thread.sleep(200)
    }
    println()
    Await.result(Future.sequence(fs), Duration.Inf)
  }

  def main(args: Array[String]): Unit = {
    println("Simulation du ratio brûlé pour différentes valeurs de mW et vitesses de vent...")

    val futures = for {
      mW <- mWValues
      v <- windSpeeds
    } yield Future { runSimFor(mW, v) }

    val results: Seq[(Double, Double, Double)] = waitWithProgress(futures)

    val sorted = results.sortBy { case (mW, v, _) => (mW, v) }

    val output = new java.io.PrintWriter("burned_vs_windspeed_vs_mW.csv")
    output.println("mW;vent;ratio")
    for ((mW, v, ratio) <- sorted) {
      output.println(f"${mW}%.2f;${v}%.2f;${ratio}%.8f".replace('.', ','))
    }
    output.close()

    println("Export terminé (burned_vs_windspeed_vs_mW.csv) !")
  }
}
