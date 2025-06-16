package Simulations

import fire.model.CellState.Burned
import fire.model.{Grid, Meteo, Simulation}
import java.io.PrintWriter

object WindForceSimulation2 {

  val gridSizeX = 40
  val gridSizeY = 140
  val nSteps = 11000

  val tempValues: Seq[Double] =
    (BigDecimal(273) to BigDecimal(323) by BigDecimal(10)).map(_.toDouble)

  val vMinGlobal = 0.0
  val vMaxGlobal = 40.0
  val precision = 0.01

  // 🔥 Crée une grille initiale avec température imposée
  def initialGrid(temperature: Double): Grid = {
    val g = Grid.initializeGrid(gridSizeX, gridSizeY)
    val updatedCells = g.cells.map(_.map(cell => cell.copy(T = temperature)))
    val fireI = gridSizeX / 3
    val fireJ = gridSizeY / 2
    val burningCell = updatedCells(fireI)(fireJ).copy(state = fire.model.CellState.Burning, T = 1850.0)
    val updatedRow = updatedCells(fireI).updated(fireJ, burningCell)
    val finalCells = updatedCells.updated(fireI, updatedRow)
    g.copy(cells = finalCells)
  }

  // 📉 Calcule le ratio de cellules brûlées
  def burnedRatio(grid: Grid): Double = {
    val total = grid.size_X * grid.size_Y
    val burned = grid.cells.flatten.count(_.state == Burned)
    burned.toDouble / total
  }

  // ⏹️ Simule étape par étape jusqu'à extinction du feu ou limite atteinte
  def runUntilExtinctionOrMax(sim: Simulation, maxSteps: Int): Grid = {
    var grid = sim.initialGrid
    var step = 0
    while (step < maxSteps && grid.cells.flatten.exists(_.state == fire.model.CellState.Burning)) {
      grid = Grid.step(grid, sim.meteo)
      step += 1
    }
    grid
  }

  // 🔬 Lance une simulation pour un couple (temp, vent)
  def runSimFor(temp: Double, v: Double): Double = {
    val meteo = Meteo((0, -1), v, 1.0)
    val sim = new Simulation(initialGrid(temp), meteo)
    val lastGrid = runUntilExtinctionOrMax(sim, nSteps)
    burnedRatio(lastGrid)
  }

  // 🔎 Recherche dichotomique du vent critique
  def findCriticalAndLog(temp: Double, writer: PrintWriter): Unit = {
    var vMin = vMinGlobal
    var vMax = vMaxGlobal
    var vMid = (vMin + vMax) / 2.0

    while ((vMax - vMin) > precision) {
      val ratio = runSimFor(temp, vMid)
      writer.println(f"$temp%.2f;$vMid%.4f;$ratio%.8f".replace('.', ','))
      writer.flush()
      println(f"T = $temp%.1f K, v = $vMid%.4f m/s → ratio = $ratio%.4f")
      if (ratio >= 0.5) vMin = vMid else vMax = vMid
      vMid = (vMin + vMax) / 2.0
    }

    val finalRatio = runSimFor(temp, vMid)
    writer.println(f"$temp%.2f;$vMid%.4f;$finalRatio%.8f".replace('.', ','))
    writer.flush()
    println(f"[✓] T = $temp%.1f K, vCrit ≈ $vMid%.4f → ratio = $finalRatio%.4f")
  }

  // 📊 Barre de progression
  def progressBar(progress: Double, width: Int = 40): String = {
    val filled = (progress * width).toInt
    val bar = "=" * filled + " " * (width - filled)
    f"[$bar] ${progress * 100}%.1f%%"
  }

  // 🚀 Point d'entrée principal
  def main(args: Array[String]): Unit = {
    println("Recherche dichotomique du vent critique pour chaque température...")

    val writer = new PrintWriter("burned_vs_windspeed_vs_Temp.csv")
    writer.println("temp;vent;ratio")

    val total = tempValues.length
    for ((temp, i) <- tempValues.zipWithIndex) {
      println(progressBar(i.toDouble / total))
      findCriticalAndLog(temp, writer)
      System.gc() // libération explicite (facultatif)
    }

    writer.close()
    println("✅ Export terminé : burned_vs_windspeed_vs_Temp.csv")
  }
}
