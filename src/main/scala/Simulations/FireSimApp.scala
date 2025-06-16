package Simulations

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.geometry.Insets
import scalafx.application.Platform
import scalafx.scene.text.Font
import fire.model.{CellState, CellStateType, Grid, Meteo, Cell => FireCell}
import fire.model.CellState._
import fire.model.Simulation

import java.io.{BufferedWriter, FileWriter}

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

object FireSimApp extends JFXApp3 {

  var simulation: Simulation = _

  var gridSizeX = 40
  var gridSizeY = 100
  var currentGrid: Grid = _
  var simulationRunning = false
  var fireOrigin: Option[(Int, Int)] = None
  var gridPane: GridPane = _
  var tempGridPane: GridPane = _
  var mWGridPane: GridPane = _
  var mGridPane: GridPane = _

  val cellSize = 4

  var mWInitial: Double = 0.06

  // Météo
  var windDirI = 0
  var windDirJ = 0
  var windSpeed = 0.0
  var dt = 1.0

  // UI
  var tempInput: TextField = _
  var mInitInput: TextField = _
  var mWInput: TextField = _
  var dtInput: TextField = _
  var windSpeedInput: TextField = _
  var windDirChoice: ComboBox[String] = _
  var stateLabel: Label = _
  var selectedCellLabel: Label = _
  var stopBtn: Button = _
  var finishBtn: Button = _

  var lastClickedCell: Option[(Int, Int)] = None

  def colorForTemperature(T: Double): Color = {
    val T_min = 250
    val T_max = 2000.0
    val clampedT = math.max(T_min, math.min(T, T_max))
    val logMin = math.log10(T_min)
    val logMax = math.log10(T_max)
    val logT = math.log10(clampedT)
    val ratio = (logT - logMin) / (logMax - logMin)
    if (ratio < 0.25) {
      val t = ratio / 0.25
      Color.color(t * 0.5, 0, 1)
    } else if (ratio < 0.5) {
      val t = (ratio - 0.25) / 0.25
      Color.color(t, 0, 1 - t)
    } else if (ratio < 0.75) {
      val t = (ratio - 0.5) / 0.25
      Color.color(1, t * 0.5, 0)
    } else {
      val t = (ratio - 0.75) / 0.25
      Color.color(1, 0.5 + t * 0.5, t * 0.5)
    }
  }

  def colorForMW(mW: Double): Color = {
    val min = 0.0
    val max = math.max(0.0001, mWInitial)
    val clamped = math.max(min, math.min(mW, max))
    val ratio = if (max > min) (clamped - min) / (max - min) else 0.0
    val r = ratio
    val red = r * 1.0 + (1 - r) * 0.0
    val green = r * 1.0 + (1 - r) * 0.7
    val blue = 0.0
    Color.color(red, green, blue)
  }

  def colorForM(m: Double): Color = {
    val mMin = 0.0
    val mMax = mInitInput.text.value.toDoubleOption.getOrElse(2.8)
    val seuilBrule = 0.02
    val clamped = math.max(mMin, math.min(m, mMax))
    val ratio = (clamped - mMin) / (mMax - mMin)
    if (m <= seuilBrule) {
      Color.Black
    } else {
      Color.color(0, ratio, 0)
    }
  }

  def colorForState(state: CellStateType): Color = state match {
    case Unburned => Color.DarkGreen
    case Heating  => Color.Yellow
    case Igniting => Color.Orange
    case Burning  => Color.Red
    case Torched  => Color.Magenta
    case Burned   => Color.Black
    case _        => Color.Gray
  }

  def updateCellInfo(i: Int, j: Int): Unit = {
    val cell = currentGrid.cells(i)(j)
    val txt = f"""
                 |Cellule [$i,$j] :
                 | État        : ${cell.state}
                 | Température : ${cell.T}%.2f K
                 | Masse       : ${cell.m}%.3f kg/m² (init: ${cell.mInit}%.2f)
                 | Humidité (en kg_eau / kg) : ${cell.mW}%.2f en pourcentage
      """.stripMargin
    Platform.runLater { selectedCellLabel.text = txt }
  }

  def updateGridDisplay(): Unit = {
    Platform.runLater {
      gridPane.children.clear()
      tempGridPane.children.clear()
      mWGridPane.children.clear()
      mGridPane.children.clear()

      for (i <- 0 until gridSizeX; j <- 0 until gridSizeY) {
        val cell = currentGrid.cells(i)(j)

        val rect = new Rectangle {
          width = cellSize
          height = cellSize
          fill = colorForState(cell.state)
          stroke = Color.Gray
        }
        rect.onMouseClicked = _ => {
          lastClickedCell = Some((i, j))
          updateCellInfo(i, j)
          if (!simulationRunning && fireOrigin.isEmpty) {
            currentGrid = setCellBurning(currentGrid, i, j, tempInput.text.value.toDoubleOption.getOrElse(1850.0))
            fireOrigin = Some((i, j))
            updateGridDisplay()
            stateLabel.text = s"Feu initié en [$i,$j]. Clique 'Démarrer' !"
          }
        }
        gridPane.add(rect, j, i)

        val tempRect = new Rectangle {
          width = cellSize
          height = cellSize
          fill = colorForTemperature(cell.T)
          strokeWidth = 0
        }
        tempGridPane.add(tempRect, j, i)

        val mwRect = new Rectangle {
          width = cellSize
          height = cellSize
          fill = colorForMW(cell.mW)
          strokeWidth = 0
        }
        mWGridPane.add(mwRect, j, i)

        val mRect = new Rectangle {
          width = cellSize
          height = cellSize
          fill = colorForM(cell.m)
          strokeWidth = 0
        }
        mGridPane.add(mRect, j, i)
      }
      lastClickedCell.foreach { case (i, j) => updateCellInfo(i, j) }
    }
  }

  def setCellBurning(grid: Grid, i: Int, j: Int, T: Double): Grid = {
    val row = grid.cells(i).updated(j, grid.cells(i)(j).copy(state = Burning, T = T))
    grid.copy(cells = grid.cells.updated(i, row))
  }

  def resetGrid(): Unit = {
    gridSizeX = gridSizeInputX.text.value.toIntOption.getOrElse(40)
    gridSizeY = gridSizeInputY.text.value.toIntOption.getOrElse(140)
    val mInit = mInitInput.text.value.toDoubleOption.getOrElse(2.8)
    val mW = mWInput.text.value.toDoubleOption.getOrElse(0.06)
    mWInitial = mW
    val z = 18.0
    val h_gap = 4.0
    val cells = (0 until gridSizeX).toList.map { i =>
      (0 until gridSizeY).toList.map { j =>
        FireCell.default(i, j).copy(m = mInit, mInit = mInit, mW = mW)
      }
    }
    currentGrid = Grid(gridSizeX, gridSizeY, cells)
    fireOrigin = None
    simulationRunning = false
    lastClickedCell = None
    selectedCellLabel.text = "Sélectionne une cellule pour voir ses infos."
    stateLabel.text = "Clique sur une cellule pour démarrer le feu…"
    updateGridDisplay()
  }

  var gridSizeInputX: TextField = _
  var gridSizeInputY: TextField = _

  // ---- LEGENDES ----
  def colorLegend(label: String, min: Double, max: Double, colorMap: Double => Color, height: Int = 100, width: Int = 24, fmt: Double => String = _.toString): VBox = {
    val canvas = new Canvas(width, height)
    val gc: GraphicsContext = canvas.graphicsContext2D
    for (i <- 0 until height) {
      val ratio = 1.0 - i.toDouble / (height - 1)
      val value = min + ratio * (max - min)
      val c = colorMap(value)
      gc.setFill(c)
      gc.fillRect(0, i, width, 1)
    }
    new VBox(2,
      new Label(label) { font = Font(12); alignment = scalafx.geometry.Pos.Center },
      new Label(fmt(max)) { font = Font(10) },
      canvas,
      new Label(fmt(min)) { font = Font(10) }
    ) { padding = Insets(2) }
  }

  override def start(): Unit = {
    tempInput = new TextField { promptText = "Temp. feu (K)"; text = "1850" }
    mInitInput = new TextField { promptText = "Masse (kg/m²)"; text = "2.8" }
    mWInput = new TextField { promptText = "Humidité (en kg_eau / kg)"; text = "0.06" }
    dtInput = new TextField { promptText = "Δt (s)"; text = "1.0" }
    gridSizeInputX = new TextField { promptText = "Nb lignes"; text = gridSizeX.toString }
    gridSizeInputY = new TextField { promptText = "Nb colonnes"; text = gridSizeY.toString }
    windSpeedInput = new TextField { promptText = "Vitesse du vent (m/s)"; text = windSpeed.toString }
    windDirChoice = new ComboBox[String](Seq(
      "aucun", "nord (↑)", "sud (↓)", "est (→)", "ouest (←)"
    )) { value = "aucun" }
    stateLabel = new Label("Clique sur une cellule pour démarrer le feu…") { font = Font(15) }
    selectedCellLabel = new Label("Sélectionne une cellule pour voir ses infos.") { font = Font(13); wrapText = true }

    gridPane = new GridPane { hgap = 0; vgap = 0; padding = Insets(0) }
    tempGridPane = new GridPane { hgap = 0; vgap = 0; padding = Insets(0) }
    mWGridPane = new GridPane { hgap = 0; vgap = 0; padding = Insets(0) }
    mGridPane = new GridPane { hgap = 0; vgap = 0; padding = Insets(0) }

    stopBtn = new Button("Stop simulation") { disable = true }
    finishBtn = new Button("Finir simulation") { disable = true }

    stopBtn.onAction = _ => {
      simulationRunning = false
      stateLabel.text = "Simulation stoppée (tu peux recommencer)."
      stopBtn.disable = true
      finishBtn.disable = true
    }

    finishBtn.onAction = _ => {
      simulationRunning = false
      stateLabel.text = "Simulation arrêtée (fin forcée)."
      stopBtn.disable = true
      finishBtn.disable = true
    }

    val resetBtn = new Button("Réinitialiser")
    resetBtn.onAction = _ => resetGrid()

    val startBtn = new Button("Démarrer simulation")
    startBtn.onAction = _ => {
      if (fireOrigin.isEmpty) {
        stateLabel.text = "Clique d'abord sur une cellule pour démarrer le feu!"
      } else if (!simulationRunning) {
        val (newI, newJ) = windDirChoice.value.value match {
          case s if s.contains("nord") => (1, 0)
          case s if s.contains("sud") => (-1, 0)
          case s if s.contains("est") => (0, -1)
          case s if s.contains("ouest") => (0, 1)
          case _ => (0, 0)
        }
        windDirI = newI
        windDirJ = newJ
        windSpeed = windSpeedInput.text.value.toDoubleOption.getOrElse(0.0)
        dt = dtInput.text.value.toDoubleOption.getOrElse(1.0)

        simulationRunning = true
        stopBtn.disable = false
        finishBtn.disable = false
        stateLabel.text = "Simulation en cours…"

        val meteo = Meteo((windDirI, windDirJ), windSpeed, dt)
        simulation = new Simulation(currentGrid, meteo)

        new Thread(() => {
          writeCSVHeader("export_simulation.csv")
          val gridIter = simulation.allSteps().iterator

          var step = 0
          while (simulationRunning && step < 10000 && gridIter.hasNext) {
            val nextGrid = gridIter.next()
            currentGrid = nextGrid
            saveGridStepToCSV("export_simulation.csv", step, currentGrid)
            Thread.sleep(50)
            updateGridDisplay()
            if (!currentGrid.cells.flatten.exists(c =>
              c.state == Burning || c.state == Igniting || c.state == Heating || c.state == Torched
            )) {
              simulationRunning = false
              Platform.runLater {
                stateLabel.text = s"Simulation terminée à step $step."
                stopBtn.disable = true
                finishBtn.disable = true
              }
            }
            step += 1
          }
        }).start()
      }
    }

    resetGrid()

    val paramPane = new GridPane {
      hgap = 8; vgap = 8; padding = Insets(10)
      add(new Label("Nb lignes:"), 0, 0); add(gridSizeInputX, 1, 0)
      add(new Label("Nb colonnes:"), 0, 1); add(gridSizeInputY, 1, 1)
      add(new Label("Temp. feu (K):"), 0, 2); add(tempInput, 1, 2)
      add(new Label("Masse (kg/m²):"), 0, 3); add(mInitInput, 1, 3)
      add(new Label("Humidité (en kg_eau / kg):"), 0, 4); add(mWInput, 1, 4)
      add(new Label("Δt (s):"), 0, 5); add(dtInput, 1, 5)
      add(new Label("Vent (direction):"), 0, 6); add(windDirChoice, 1, 6)
      add(new Label("Vent (m/s):"), 0, 7); add(windSpeedInput, 1, 7)
      add(startBtn, 0, 8, 2, 1)
      add(resetBtn, 0, 9, 2, 1)
      add(stopBtn, 0, 10, 2, 1)
      add(finishBtn, 0, 11, 2, 1)
    }

    val legendTemp = colorLegend("Temp. (K)", 250, 2000, colorForTemperature, fmt = v => f"$v%.0f")
    val legendMW = colorLegend("mW", 0.0, mWInitial, colorForMW, fmt = v => f"$v%.2f")
    val legendM = colorLegend("m (kg/m²)", 0.0, 5.0, colorForM, fmt = v => f"$v%.2f")

    val verticalGridsWithLegends = new VBox(10,
      new HBox(8, new VBox(2, new Label("Température (K)"), tempGridPane), legendTemp),
      new HBox(8, new VBox(2, new Label("Humidité (en kg_eau / kg) mW"), mWGridPane), legendMW),
      new HBox(8, new VBox(2, new Label("Masse m"), mGridPane), legendM)
    )

    val sceneWidth = (cellSize + 2) * gridSizeY + 400
    val sceneHeight = (cellSize + 2) * gridSizeX + 120

    stage = new JFXApp3.PrimaryStage {
      title = "Simulation propagation feu - ScalaFX"
      scene = new Scene(sceneWidth, sceneHeight) {
        root = new HBox(18,
          new VBox(10, paramPane, stateLabel, selectedCellLabel),
          new VBox(5, new Label("État des cellules"), gridPane),
          verticalGridsWithLegends
        )
      }
    }
  }
}
