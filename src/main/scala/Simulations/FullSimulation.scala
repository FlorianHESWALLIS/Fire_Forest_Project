package Simulations

import fire.model.CellState.Burned
import fire.model.{Grid, Meteo, Simulation}
import java.io.PrintWriter
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.*

object FullSimulation {

  val gridSizeX = 40
  val gridSizeY = 140
  val nSteps = 11000

  // === PARAMÈTRES PERSONNALISABLES ===
  val tempMin = 273.0
  val tempMax = 323.0
  val tempStep = 5.0

  val mWMin = 0.00
  val mWMax = 1.00
  val mWStep = 0.02

  val windMin = 0.0
  val windMax = 32.0
  val windStep = 0.5

  val tempValues: Seq[Double] =
    (BigDecimal(tempMin) to BigDecimal(tempMax) by BigDecimal(tempStep)).map(_.toDouble)
  val mWValues: Seq[Double] =
    (BigDecimal(mWMin) to BigDecimal(mWMax) by BigDecimal(mWStep)).map(_.toDouble)
  val windSpeeds: Seq[Double] =
    (BigDecimal(windMin) to BigDecimal(windMax) by BigDecimal(windStep)).map(_.toDouble)

  // 🔥 Grille initiale avec mW et température imposés
  def initialGrid(mW: Double, temp: Double): Grid = {
    val g = Grid.initializeGrid(gridSizeX, gridSizeY)
    val updatedCells = g.cells.map(_.map(cell => cell.copy(mW = mW, T = temp)))
    val fireI = gridSizeX / 3
    val fireJ = gridSizeY / 2
    val fireCell = updatedCells(fireI)(fireJ).copy(state = fire.model.CellState.Burning, T = 1850.0)
    val finalRow = updatedCells(fireI).updated(fireJ, fireCell)
    g.copy(cells = updatedCells.updated(fireI, finalRow))
  }

  // ✅ Calcule le ratio brûlé
  def burnedRatio(grid: Grid): Double = {
    val total = grid.size_X * grid.size_Y
    val burned = grid.cells.flatten.count(_.state == Burned)
    burned.toDouble / total
  }

  // 🔁 Avance pas à pas jusqu'à extinction ou max steps
  def runUntilExtinctionOrMax(sim: Simulation, maxSteps: Int): Grid = {
    var grid = sim.initialGrid
    var step = 0
    while (step < maxSteps && grid.cells.flatten.exists(_.state == fire.model.CellState.Burning)) {
      grid = Grid.step(grid, sim.meteo)
      step += 1
    }
    grid
  }

  // 🔬 Simulation pour un triplet (mW, temp, vent)
  def runSimFor(mW: Double, temp: Double, v: Double): (Double, Double, Double, Double) = {
    val meteo = Meteo((0, -1), v, 1.0)
    val sim = new Simulation(initialGrid(mW, temp), meteo)
    val lastGrid = runUntilExtinctionOrMax(sim, nSteps)
    val ratio = burnedRatio(lastGrid)
    (mW, temp, v, ratio)
  }

  // 📊 Barre de progression
  def progressBar(progress: Double, width: Int = 40): String = {
    val filled = (progress * width).toInt
    val bar = "=" * filled + " " * (width - filled)
    f"[$bar] ${progress * 100}%.1f%%"
  }

  // 🚀 Programme principal
  def main(args: Array[String]): Unit = {
    println("Simulation complète pour différentes températures, humidités et vitesses de vent...")

    // Détection automatique du nombre de threads à utiliser
    val totalLogical = Runtime.getRuntime.availableProcessors()
    val nbThreads = math.min(12, math.max(4, totalLogical - 2))
    val executor = Executors.newFixedThreadPool(nbThreads)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(executor)
    val output = new PrintWriter("burned_vs_windspeed_vs_Temp_vs_mW.csv")
    output.println("mW;temp;vent;ratio")

    val allCombinations = for {
      mW <- mWValues
      temp <- tempValues
      v <- windSpeeds
    } yield (mW, temp, v)

    val total = allCombinations.size
    var done = 0

    val futures = allCombinations.map { case (mW, temp, v) =>
      Future {
        val result = runSimFor(mW, temp, v)
        synchronized {
          done += 1
          print("\r" + progressBar(done.toDouble / total))
          output.println(f"${result._1}%.8f;${result._2}%.8f;${result._3}%.8f;${result._4}%.8f".replace('.', ','))
          output.flush()
        }
      }
    }

    Await.result(Future.sequence(futures), Duration.Inf)
    output.close()
    println("\n✅ Export terminé : burned_vs_windspeed_vs_Temp_vs_mW.csv")
    executor.shutdown()
  }
}
