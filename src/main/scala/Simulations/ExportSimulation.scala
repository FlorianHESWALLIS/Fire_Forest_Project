import fire.model.{Grid, Meteo, Simulation, Cell => FireCell}
import fire.model.CellState
import java.io.{BufferedWriter, FileWriter}
import java.awt.image.BufferedImage
import java.awt.{Color => AwtColor}
import javax.imageio.ImageIO
import java.io.File
import fire.model.{CellState, CellStateType}


object ExportSimulation {

  def writeCSVHeader(file: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(file, false))
    writer.write("step,i,j,state,T,m,mInit,mW\n")
    writer.close()
  }

  def saveGridStepToCSV(file: String, step: Int, grid: Grid): Unit = {
    val writer = new BufferedWriter(new FileWriter(file, true))
    for {
      i <- 0 until grid.size_X
      j <- 0 until grid.size_Y
      cell = grid.cells(i)(j)
    } writer.write(
      s"$step,$i,$j,${cell.state},${cell.T},${cell.m},${cell.mInit},${cell.mW}\n"
    )
    writer.close()
  }

  // Couleurs - même logique que FireSimViewer.scala
  def colorForState(state: CellStateType): AwtColor = state match {
    case CellState.Unburned => new AwtColor(0, 100, 0)        // DarkGreen
    case CellState.Heating  => new AwtColor(255, 255, 0)      // Yellow
    case CellState.Igniting => new AwtColor(255, 165, 0)      // Orange
    case CellState.Burning  => new AwtColor(255, 0, 0)        // Red
    case CellState.Torched  => new AwtColor(255, 0, 255)      // Magenta
    case CellState.Burned   => new AwtColor(0, 0, 0)          // Black
    case _                  => new AwtColor(128, 128, 128)    // Gray
  }
  def colorForTemperature(T: Double): AwtColor = {
    val T_min = 250
    val T_max = 2000.0
    val clampedT = math.max(T_min, math.min(T, T_max))
    val logMin = math.log10(T_min)
    val logMax = math.log10(T_max)
    val logT = math.log10(clampedT)
    val ratio = (logT - logMin) / (logMax - logMin)
    if (ratio < 0.25) {
      val t = ratio / 0.25
      new AwtColor((t * 0.5 * 255).toInt, 0, 255)
    } else if (ratio < 0.5) {
      val t = (ratio - 0.25) / 0.25
      new AwtColor((t * 255).toInt, 0, ((1 - t) * 255).toInt)
    } else if (ratio < 0.75) {
      val t = (ratio - 0.5) / 0.25
      new AwtColor(255, (t * 0.5 * 255).toInt, 0)
    } else {
      val t = (ratio - 0.75) / 0.25
      new AwtColor(
        255,
        ((0.5 + t * 0.5) * 255).toInt,
        (t * 0.5 * 255).toInt
      )

    }
  }
  def colorForMW(mW: Double, mWInitial: Double): AwtColor = {
    val min = 0.0
    val max = math.max(0.0001, mWInitial)
    val clamped = math.max(min, math.min(mW, max))
    val ratio = if (max > min) (clamped - min) / (max - min) else 0.0
    val r = ratio
    val red = (r * 255).toInt
    val green = (r * 255 + (1 - r) * 178).toInt
    val blue = 0
    new AwtColor(red, green, blue)
  }
  def colorForM(m: Double, mInit: Double): AwtColor = {
    val mMin = 0.0
    val mMax = mInit
    val seuilBrule = 0.02
    val clamped = math.max(mMin, math.min(m, mMax))
    val ratio = (clamped - mMin) / (mMax - mMin)
    if (m <= seuilBrule) {
      new AwtColor(0, 0, 0)
    } else {
      new AwtColor(0, (ratio * 255).toInt, 0)
    }
  }

  def saveGridStepImage(
                         step: Int,
                         grid: Grid,
                         mWInitial: Double,
                         dataDir: String,
                         mInit: Double
                       ): Unit = {
    val width = grid.size_Y
    val height = grid.size_X

    // 4 images par step (état, température, mW, m)
    val imgState = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val imgTemp  = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val imgMW    = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val imgM     = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for {
      i <- 0 until height
      j <- 0 until width
      cell = grid.cells(i)(j)
    } {
      imgState.setRGB(j, i, colorForState(cell.state).getRGB)
      imgTemp.setRGB(j, i, colorForTemperature(cell.T).getRGB)
      imgMW.setRGB(j, i, colorForMW(cell.mW, mWInitial).getRGB)
      imgM.setRGB(j, i, colorForM(cell.m, mInit).getRGB)
    }
    // Sauvegarde dans dataDir
    ImageIO.write(imgState, "png", new File(s"$dataDir/step_${step}_state.png"))
    ImageIO.write(imgTemp, "png",  new File(s"$dataDir/step_${step}_temp.png"))
    ImageIO.write(imgMW,   "png",  new File(s"$dataDir/step_${step}_mw.png"))
    ImageIO.write(imgM,    "png",  new File(s"$dataDir/step_${step}_m.png"))
  }

  def main(args: Array[String]): Unit = {
    // --- Paramètres à personnaliser ou à passer en args
    val gridSizeX = 40
    val gridSizeY = 100
    val tempFeu = 1850.0
    val mInit = 2.8
    val mW = 0.06
    val windDir = (-1, 0)
    val windSpeed = 20.0
    val dt = 1.0
    val maxSteps = 10000

    val baseName = f"export_simulation_${gridSizeX}x${gridSizeY}_mInit_${mInit}_mW_${mW}_Wind_${windDir}_${windSpeed}"
    val outputFile = s"src/main/scala/Simulations/Data/$baseName.csv"
    val dataDir = "src/main/scala/Simulations/Data"

    // Création du dossier s'il n'existe pas
    new java.io.File(dataDir).mkdirs()

    // --- Grille initiale
    val cells = (0 until gridSizeX).toList.map { i =>
      (0 until gridSizeY).toList.map { j =>
        FireCell.default(i, j).copy(m = mInit, mInit = mInit, mW = mW)
      }
    }
    var grid = Grid(gridSizeX, gridSizeY, cells)
    // Allumer le feu au centre
    val i0 = gridSizeX / 2
    val j0 = gridSizeY / 3
    val row = grid.cells(i0).updated(j0, grid.cells(i0)(j0).copy(state = fire.model.CellState.Burning, T = tempFeu))
    grid = grid.copy(cells = grid.cells.updated(i0, row))

    val meteo = Meteo(windDir, windSpeed, dt)
    val sim = new Simulation(grid, meteo)
    val gridIter = sim.allSteps().iterator

    writeCSVHeader(outputFile)
    var step = 0
    var current = grid

    while (step < maxSteps && gridIter.hasNext) {
      current = gridIter.next()
      saveGridStepToCSV(outputFile, step, current)
      saveGridStepImage(step, current, mW, dataDir, mInit)
      if (!current.cells.flatten.exists(c =>
        c.state == CellState.Burning || c.state == CellState.Igniting || c.state == CellState.Heating || c.state == CellState.Torched
      )) {
        println(s"Fin de la simulation à step $step.")
        return
      }
      step += 1
      if (step % 100 == 0) println(s"Step $step terminé.")
    }
    println(s"Simulation exportée jusqu'à $step steps.")
  }
}
