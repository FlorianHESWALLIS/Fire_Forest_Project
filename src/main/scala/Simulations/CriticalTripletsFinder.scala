package Simulations

import fire.model.{Grid, Meteo, Simulation, CellState}
import java.io.PrintWriter
import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

object CriticalTripletsFinder {

  val gridSizeX = 40
  val gridSizeY = 140
  val nSteps = 11000

  val tempMin = 293.0
  val tempMax = 294.0
  val tempStep = 1.0

  val mWMin = 0.00
  val mWMax = 1.00
  val mWStep = 0.1

  val windMin = 24.0
  val windMax = 27.5
  val windStepInit = 0.1
  val windPrecision = 0.1

  val tempValues = (BigDecimal(tempMin) to BigDecimal(tempMax) by BigDecimal(tempStep)).map(_.toDouble)
  val mWValues = (BigDecimal(mWMin) to BigDecimal(mWMax) by BigDecimal(mWStep)).map(_.toDouble)

  def initialGrid(mW: Double, temp: Double): Grid = {
    val g = Grid.initializeGrid(gridSizeX, gridSizeY)
    val updatedCells = g.cells.map(_.map(cell => cell.copy(mW = mW, T = temp)))
    val fireI = gridSizeX / 3
    val fireJ = gridSizeY / 2
    val fireCell = updatedCells(fireI)(fireJ).copy(state = CellState.Burning, T = 1850.0)
    val finalRow = updatedCells(fireI).updated(fireJ, fireCell)
    g.copy(cells = updatedCells.updated(fireI, finalRow))
  }

  def burnedRatio(grid: Grid): Double = {
    val total = grid.size_X * grid.size_Y
    val burned = grid.cells.flatten.count(_.state == CellState.Burned)
    burned.toDouble / total
  }

  def runUntilExtinctionOrMax(sim: Simulation, maxSteps: Int): Grid = {
    Iterator.iterate((sim.initialGrid, 0)) { case (grid, step) =>
        (Grid.step(grid, sim.meteo), step + 1)
      }
      .dropWhile { case (grid, step) =>
        step < maxSteps && grid.cells.flatten.exists(_.state == CellState.Burning)
      }
      .next()._1
  }

  def runSimFor(mW: Double, temp: Double, v: Double): Double = {
    val meteo = Meteo((0, -1), v, 1.0)
    val sim = new Simulation(initialGrid(mW, temp), meteo)
    val lastGrid = runUntilExtinctionOrMax(sim, nSteps)
    burnedRatio(lastGrid)
  }

  def estimateVcritBounds(
                           temp: Double,
                           mW: Double,
                           history: List[(Double, Double)],
                           varying: String
                         ): (Double, Double) = {
    val (vEst, formule) =
      if (history.size >= 2) {
        val (x1, v1) = history(history.size - 2)
        val (x2, v2) = history(history.size - 1)
        val a = (v2 - v1) / (x2 - x1)
        val vPred = v2 + (if (varying == "mW") mW - x2 else temp - x2) * a
        (vPred, f"pente locale: a=$a%.3f")
      } else {
        if (varying == "mW") {
          val vPred = -2.8273 * mW + 27.1227
          (vPred, "modèle empirique mW")
        } else {
          val vPred = 0.1472 * temp - 16.0349
          (vPred, "modèle empirique Temp")
        }
      }
    println(f"[$temp%.2f K, $mW%.3f mW] => Estimation $formule v_crit = $vEst%.3f")
    val vStart = math.max(windMin, vEst - windStepInit)
    val vEnd = math.min(windMax, vEst + windStepInit)
    (vStart, vEnd)
  }

  def seekValidInterval(mW: Double, temp: Double, vmin: Double, vmax: Double, step: Double = 0.1): Option[(Double, Double)] = {
    var v = vmin
    var lastRatio = runSimFor(mW, temp, v)
    var lastV = v
    var found = false
    while (v <= vmax && !found) {
      val ratio = runSimFor(mW, temp, v)
      println(f"    Balayage v = $v%.3f : ratio = $ratio%.3f")
      if (lastRatio > 0.5 && ratio <= 0.5) found = true
      else {
        lastV = v
        lastRatio = ratio
        v += step
      }
    }
    if (found) Some((lastV, v)) else None
  }

  @annotation.tailrec
  def findVCritDicho(
                      temp: Double,
                      mW: Double,
                      vLow: Double,
                      vHigh: Double,
                      ratioLow: Double,
                      ratioHigh: Double,
                      precision: Double
                    ): Double = {
    if ((vHigh - vLow) < precision) vHigh
    else {
      val vMid = (vLow + vHigh) / 2.0
      val ratioMid = runSimFor(mW, temp, vMid)
      println(f"    Test v=$vMid%.3f : ratio = $ratioMid%.3f")
      if (ratioMid > 0.5) findVCritDicho(temp, mW, vMid, vHigh, ratioMid, ratioHigh, precision)
      else findVCritDicho(temp, mW, vLow, vMid, ratioLow, ratioMid, precision)
    }
  }

  def printProgress(done: Int, total: Int, width: Int = 40): Unit = {
    val percent = done * 100.0 / total
    val filled = (percent / 100.0 * width).toInt
    val bar = "=" * filled + " " * (width - filled)
    print(f"\rProgression : [$bar] $percent%5.1f%%  ($done/$total)")
  }

  def main(args: Array[String]): Unit = {
    val nbCpus = Runtime.getRuntime.availableProcessors()
    val nbThreads = math.max(2, nbCpus - 1)
    val executor = Executors.newFixedThreadPool(nbThreads)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

    val output = new PrintWriter("critical_triplets.csv")
    output.println("temp;mW;v_critique;ratio_avant;ratio_apres")

    // On détecte quelle variable est "primaire"
    val varying = if (mWValues.size >= tempValues.size) "mW" else "Temp"
    val primaryVals = if (varying == "mW") tempValues else mWValues
    val secondaryVals = if (varying == "mW") mWValues else tempValues

    val total = primaryVals.size * secondaryVals.size
    var done = 0

    // foldLeft sur les valeurs primaires, puis sur les secondaires
    val results = primaryVals.flatMap { tOrMw =>
      // On démarre avec une liste vide pour l'historique
      val (_, listRes) = secondaryVals.foldLeft((List.empty[(Double, Double)], List.empty[((Double, Double, Double), Double, Double)])) {
        case ((history, acc), sec) =>
          val (temp, mW) = if (varying == "mW") (tOrMw, sec) else (sec, tOrMw)
          println(f"\n--- [T=$temp%.2f K, mW=$mW%.3f] ---")
          val (vStart0, vEnd0) = estimateVcritBounds(temp, mW, history, varying)
          val initialRatioStart = runSimFor(mW, temp, vStart0)
          val initialRatioEnd = runSimFor(mW, temp, vEnd0)
          println(f"[$temp%.2f, $mW%.3f] vStart = $vStart0%.3f (ratio = $initialRatioStart%.3f), vEnd = $vEnd0%.3f (ratio = $initialRatioEnd%.3f)")

          val validInterval =
            if (initialRatioStart > 0.5 && initialRatioEnd <= 0.5) Some((vStart0, vEnd0))
            else {
              println(f"  ⚠️  Aucun seuil détecté dans l'intervalle suggéré, balayage de tout l'intervalle...")
              seekValidInterval(mW, temp, windMin, windMax)
            }

          validInterval match {
            case None =>
              println(f"  🔴 PAS DE SEUIL dans tout l'intervalle !")
              (history, acc)
            case Some((vStart, vEnd)) =>
              println(f"  ➡️  Recherche dichotomique entre $vStart%.2f et $vEnd%.2f m/s...")
              val ratioStart = runSimFor(mW, temp, vStart)
              val ratioEnd = runSimFor(mW, temp, vEnd)
              val vCrit = findVCritDicho(temp, mW, vStart, vEnd, ratioStart, ratioEnd, windPrecision)
              val ratioBefore = runSimFor(mW, temp, vCrit - windPrecision)
              val ratioAfter = runSimFor(mW, temp, vCrit)
              println(f"✅ Point critique trouvé : [T=$temp%.2f K, mW=$mW%.3f] vCritique = $vCrit%.3f m/s (ratio avant = $ratioBefore%.3f, après = $ratioAfter%.3f)")
              val xVal = if (varying == "mW") mW else temp
              (history :+ (xVal, vCrit), acc :+ ((temp, mW, vCrit), ratioBefore, ratioAfter))
          }
      }
      // Affichage progressif (optionnel)
      done += secondaryVals.size
      printProgress(done, total)
      listRes
    }

    // Export dans le CSV
    results.foreach {
      case ((t, w, vcrit), ratioBefore, ratioAfter) =>
        output.println(f"$t%.2f;$w%.3f;$vcrit%.3f;$ratioBefore%.3f;$ratioAfter%.3f")
    }

    output.close()
    executor.shutdown()
    println("\n✅ Extraction des triplets critiques terminée.")
  }
}
