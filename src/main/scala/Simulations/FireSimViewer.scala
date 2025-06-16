package Simulations

import scalafx.application.JFXApp3
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{HBox, VBox, Priority, Region}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.text.{Font, FontWeight}
import scalafx.geometry.Insets
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.SceneIncludes.{jfxScene2sfx, jfxNode2sfx}

import java.io.File
import javax.imageio.ImageIO

// Pour accéder au Canvas JavaFX natif (pour resize dans les légendes)
import javafx.scene.canvas.Canvas => JfxCanvas

object FireSimViewer extends JFXApp3 {

  // --- Variables d’état globales ---
  var infoLabel: Label = _
  var stateLabel: Label = _
  var paramLabel: Label = _
  var imgStateView: ImageView = _
  var imgTempView: ImageView = _
  var imgMWView: ImageView = _
  var imgMView: ImageView = _
  var legendTemp: VBox = _
  var legendMW: VBox = _
  var legendM: VBox = _
  var cardsAndLegendsVBox: VBox = _
  var mainRoot: HBox = _

  var baseName = ""
  var dataDir = "src/main/scala/Simulations/Data"
  var gridSizeX = 40
  var gridSizeY = 100
  var stepMax = 0
  var currentStep = 0
  var playing = false
  var playDelayMs = 60

  // --- Fonctions utilitaires ---
  def findStepMax(prefix: String, dir: String): Int = {
    val files = new File(dir).listFiles()
    if (files == null) return 0
    val nums = files.flatMap { f =>
      val m = ("""step_(\d+)_state\.png""".r).findFirstMatchIn(f.getName)
      m.map(_.group(1).toInt)
    }
    if (nums.isEmpty) 0 else nums.max
  }

  def detectGridSize(dir: String, prefix: String): (Int, Int) = {
    val f = new File(s"$dir/${if (prefix.nonEmpty) prefix + "/" else ""}step_0_state.png")
    if (f.exists()) {
      val img = ImageIO.read(f)
      (img.getHeight, img.getWidth)
    } else (40, 100)
  }

  def loadStepImages(step: Int): (Image, Image, Image, Image) = {
    val prefixPath = s"$dataDir/${if (baseName.nonEmpty) baseName + "/" else ""}"
    val imgState = new Image(s"file:${prefixPath}step_${step}_state.png")
    val imgTemp  = new Image(s"file:${prefixPath}step_${step}_temp.png")
    val imgMW    = new Image(s"file:${prefixPath}step_${step}_mw.png")
    val imgM     = new Image(s"file:${prefixPath}step_${step}_m.png")
    (imgState, imgTemp, imgMW, imgM)
  }

  def updateStep(step: Int): Unit = {
    val (imgState, imgTemp, imgMW, imgM) = loadStepImages(step)
    Platform.runLater {
      imgStateView.image = imgState
      imgTempView.image = imgTemp
      imgMWView.image   = imgMW
      imgMView.image    = imgM
      stateLabel.text = s"Étape : $step / $stepMax"
    }
  }

  // --- Légendes ---
  def colorLegend(label: String, min: Double, max: Double, colorMap: Double => Color, height: Int = 200, width: Int = 22, fmt: Double => String = _.toString): VBox = {
    val canvas = new Canvas(width, height)
    val gc = canvas.graphicsContext2D
    for (i <- 0 until height) {
      val ratio = 1.0 - i.toDouble / (height - 1)
      val value = min + ratio * (max - min)
      val c = colorMap(value)
      gc.setFill(c)
      gc.fillRect(0, i, width, 1)
    }
    new VBox(2,
      new Label(label) { font = Font.font("Arial", FontWeight.Bold, 13); alignment = scalafx.geometry.Pos.Center },
      new Label(fmt(max)) { font = Font(10) },
      canvas,
      new Label(fmt(min)) { font = Font(10) }
    ) { padding = Insets(2, 0, 2, 6) }
  }

  // --- Couleurs ---
  def colorForTemperatureFx(T: Double): Color = {
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
  def colorForMWFx(mW: Double): Color = {
    val min = 0.0
    val max = 0.1
    val clamped = math.max(min, math.min(mW, max))
    val ratio = if (max > min) (clamped - min) / (max - min) else 0.0
    val r = ratio
    val red = r * 1.0 + (1 - r) * 0.0
    val green = r * 1.0 + (1 - r) * 0.7
    val blue = 0.0
    Color.color(red, green, blue)
  }
  def colorForMFx(m: Double): Color = {
    val mMin = 0.0
    val mMax = 2.8
    val seuilBrule = 0.02
    val clamped = math.max(mMin, math.min(m, mMax))
    val ratio = (clamped - mMin) / (mMax - mMin)
    if (m <= seuilBrule) {
      Color.Black
    } else {
      Color.color(0, ratio, 0)
    }
  }

  // --- Initialisation principale ---
  override def start(): Unit = {
    infoLabel = new Label("Visualisation PNG exportées") { font = Font(13); wrapText = true }
    stateLabel = new Label("Aucune image chargée") { font = Font(18) }
    paramLabel = new Label("Paramètres : ") { font = Font(15); wrapText = true }

    imgStateView = new ImageView { preserveRatio = false }
    imgTempView  = new ImageView { preserveRatio = false }
    imgMWView    = new ImageView { preserveRatio = false }
    imgMView     = new ImageView { preserveRatio = false }

    // Placeholders pour la première initialisation
    legendTemp = new VBox()
    legendMW   = new VBox()
    legendM    = new VBox()

    cardsAndLegendsVBox = new VBox() // Vide au départ

    // Contrôles
    val dataDirField = new TextField { text = dataDir; promptText = "Dossier PNGs" }
    val baseNameField = new TextField { text = baseName; promptText = "Préfixe nom simulation (laisser vide si aucun)" }

    val loadBtn = new Button("Charger")
    val playBtn = new Button("Lecture")
    val pauseBtn = new Button("Pause")
    val stepFwdBtn = new Button("Step +1")
    val stepBackBtn = new Button("Step -1")
    val restartBtn = new Button("Recommencer")

    val speedSlider = new Slider(10, 500, playDelayMs) {
      majorTickUnit = 100
      minorTickCount = 4
      blockIncrement = 10
      showTickLabels = true
      showTickMarks = true
      snapToTicks = true
      minWidth = 180
    }
    val speedLabel = new Label(s"Vitesse de lecture: $playDelayMs ms/étape") { font = Font(14) }
    speedSlider.value.onChange { (_, _, newVal) =>
      playDelayMs = newVal.intValue()
      speedLabel.text = s"Vitesse de lecture: $playDelayMs ms/étape"
    }

    playBtn.onAction = _ => {
      playing = true
      new Thread(() => {
        while (playing && currentStep < stepMax) {
          Thread.sleep(playDelayMs)
          currentStep += 1
          updateStep(currentStep)
        }
      }).start()
    }
    pauseBtn.onAction = _ => playing = false
    stepFwdBtn.onAction = _ => {
      if (currentStep < stepMax) { currentStep += 1; updateStep(currentStep) }
    }
    stepBackBtn.onAction = _ => {
      if (currentStep > 0) { currentStep -= 1; updateStep(currentStep) }
    }
    restartBtn.onAction = _ => { currentStep = 0; updateStep(currentStep) }

    // Contrôles (gauche)
    val controlsVBox = new VBox(12,
      new Label("Dossier PNGs :") { font = Font(14) }, dataDirField,
      new Label("Préfixe nom sim. :") { font = Font(14) }, baseNameField,
      loadBtn,
      playBtn, pauseBtn, stepBackBtn, stepFwdBtn, restartBtn,
      stateLabel, paramLabel, infoLabel, speedLabel, speedSlider
    ) {
      maxWidth = 420
      minWidth = 240
      padding = Insets(16)
      style = "-fx-background-color: #f6f6f6; -fx-border-color: #cccccc; -fx-border-width: 0 1 0 0;"
    }

    // Compose le layout principal au démarrage
    mainRoot = new HBox(8,
      controlsVBox,
      cardsAndLegendsVBox
    )

    // Affichage auto-adaptatif
    stage = new JFXApp3.PrimaryStage {
      title = "Visualiseur PNG Simulation"
      scene = new Scene(1920, 1080) {
        root = mainRoot
        fill = Color.web("#fafbfc")

        // Adapte les cartes à la taille de la fenêtre
        width.onChange { (_, _, w) =>
          resizeCards(w.doubleValue, height.value)
        }
        height.onChange { (_, _, h) =>
          resizeCards(width.value, h.doubleValue)
        }
      }
    }

    // Fonction pour resize selon la fenêtre
    def resizeCards(winW: Double, winH: Double): Unit = {
      val leftPanelW = 340
      val carteW = Math.max(320, (winW - leftPanelW - 240).toInt)
      val carteH = ((winH - 160) / 5.0).toInt
      List(imgStateView, imgTempView, imgMWView, imgMView).foreach { v =>
        v.fitWidth  = carteW
        v.fitHeight = carteH
      }

     
      def resizeLegend(vbox: VBox): Unit = {
        if (vbox != null) vbox.children.foreach { n =>
          val del = n.delegate
          del match {
            case c: JfxCanvas => c.setHeight(carteH * 0.65)
            case _ =>
          }
        }
      }
      resizeLegend(legendTemp)
      resizeLegend(legendMW)
      resizeLegend(legendM)

      if (cardsAndLegendsVBox != null) {
        cardsAndLegendsVBox.children.foreach { n =>
          if (n.isInstanceOf[HBox]) {
            val h = n.asInstanceOf[HBox]
            h.minHeight = carteH
            h.maxHeight = carteH
          }
        }
      }
    }

    // Action de chargement
    loadBtn.onAction = _ => {
      dataDir = dataDirField.text.value
      baseName = baseNameField.text.value.trim
      stepMax = findStepMax(baseName, dataDir)
      currentStep = 0

      val (nx, ny) = detectGridSize(dataDir, baseName)
      gridSizeX = nx
      gridSizeY = ny

      // Dimensions dynamiques
      val carteW = Math.max(420, ((stage.scene.value.width.value - 360 - 100).toInt))
      val carteH = ((stage.scene.value.height.value - 100) / 4.0).toInt

      List(imgStateView, imgTempView, imgMWView, imgMView).foreach { v =>
        v.fitWidth  = carteW
        v.fitHeight = carteH
      }
      legendTemp = colorLegend("Température (K)", 250, 2000, colorForTemperatureFx, height = carteH)
      legendMW   = colorLegend("mW", 0.0, 0.1, colorForMWFx, height = carteH)
      legendM    = colorLegend("m (kg/m²)", 0.0, 2.8, colorForMFx, height = carteH)

      // Fonction utilitaire pour ligne affichage + légende
      def rowCarte(imgView: ImageView, legend: Option[VBox]) = new HBox(
        imgView,
        new Region { minWidth = 24; maxWidth = 28 },
        legend.getOrElse(new VBox())
      ) {
        spacing = 0
        padding = Insets(0, 0, 0, 0)
        HBox.setHgrow(imgView, Priority.Always)
        HBox.setHgrow(this, Priority.Always)
        minHeight = carteH
        maxHeight = carteH
      }
      val rowEtat = rowCarte(imgStateView, None)
      val rowTemp = rowCarte(imgTempView, Some(legendTemp))
      val rowMW   = rowCarte(imgMWView, Some(legendMW))
      val rowM    = rowCarte(imgMView, Some(legendM))
      val vbox = new VBox(18, rowEtat, rowTemp, rowMW, rowM) {
        padding = Insets(12, 24, 12, 20)
        minWidth = carteW + 100
        maxWidth = carteW + 120
        style = "-fx-background-color: #fff;"
        VBox.setVgrow(this, Priority.Always)
      }

      Platform.runLater {
        mainRoot.children.set(1, vbox)
        cardsAndLegendsVBox = vbox
        resizeCards(stage.scene.value.width.value, stage.scene.value.height.value)
      }

      updateStep(currentStep)
      paramLabel.text = s"Dossier : $dataDir\nPréfixe : $baseName\nGrille : $gridSizeX x $gridSizeY"
    }
  }
}
