package fire.model

import fire.model.CellState._
import scala.util.Random

case class Grid(size_X: Int, size_Y: Int, cells: List[List[Cell]])

object Grid {

  /**
   * Initialise une grille vide (toutes cellules par défaut).
   */
  def initializeGrid(size_X: Int, size_Y: Int): Grid = {
    val cells = (0 until size_X).toList.map { i =>
      (0 until size_Y).toList.map { j => Cell.default(i, j) }
    }
    Grid(size_X, size_Y, cells)
  }

  /**
   * Simule une étape sur toute la grille, purement fonctionnel :

   * - Van Wagner (1977) et Sardoy (2008) pour la transition Torched.
   * - Rothermel (1972) pour l’évolution de la température, masse, intensité.
   * - Albini (1979) pour les brandons et spot fire.
   */
  def step(
            grid: Grid,
            meteo: Meteo,
            withTorching: Boolean = false,
            withBrandons: Boolean = false,
            withSpotFire: Boolean = false,
            withSardoy: Boolean = false
          ): Grid = {


    val t_MAX = 2000.0 // Température max (K)

    // Génération des nouvelles cellules
    val updatedCellsWithBrandons: List[List[(Cell, List[(Int, Int)])]] =
      grid.cells.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (cell, j) =>
          val neighbors = getMooreNeighbors(i, j, grid)

          // --- Flux thermiques (Rothermel, 1972) ---
          val (qRad, qCond, qConv) = computeFluxes(cell, neighbors, meteo)
          val dTdt = (qRad + qCond + qConv - Cell.Lv * evaporationRate(cell)) / (Cell.rho * Cell.Cp)
          val Tnew = math.min(cell.T + 5 * dTdt * meteo.dt, t_MAX)


          // --- Critères Torched : Van Wagner & Sardoy ---
          val isVanWagner = !cell.wasTorched && testTorching(cell, meteo, withTorching)
          val isSardoy = !cell.wasTorched && testSardoyTorching(cell, withSardoy)

          // Transition d'état
          val (newState, torchedStepsNew, wasTorchedNew) = cell.state match {
            case Burning =>
              if (cell.m <= 0.0001 * cell.mInit) (Burned, 0, cell.wasTorched)
              else if (isVanWagner || isSardoy) (Torched, 3, true)
              else (Burning, 0, cell.wasTorched)
            case Torched =>
              if (cell.m <= 0.01 * cell.mInit) (Burned, 0, true)
              else if (cell.torchedSteps > 1) (Torched, cell.torchedSteps - 1, true)
              else (Burning, 0, true)
            case Unburned | Heating =>
              if (Tnew >= Cell.Tign) (Igniting, 0, cell.wasTorched)
              else (Heating, 0, cell.wasTorched)
            case Igniting =>
              if (cell.m > 0) (Burning, 0, cell.wasTorched)
              else (Burned, 0, cell.wasTorched)
            case Burned => (Burned, 0, cell.wasTorched)
          }

          // --- Évolution de la masse de combustible (Rothermel, 1972) ---

          // --- PARAMÈTRE EMPIRIQUE ---
          // Influence du vent sur la combustion locale (0.0 = aucune influence, 0.05 = influence notable)
          val alpha: Double = 0.05
          
          val mNew =
            if (cell.state == Burning)
              math.max(0.0, cell.m - Cell.k * (1.0 + alpha * meteo.ventVitesse) * cell.m * meteo.dt)
            else if (cell.state == Torched)
              math.max(0.0, cell.m - 5 * Cell.k * (1.0 + alpha * meteo.ventVitesse) * cell.m * meteo.dt)
            else cell.m


          val evaporation = evaporationRate(cell) * meteo.dt
          val new_mW = math.max(0.0, cell.mW - evaporation)
          val intensity =
            if (cell.state == Burning || cell.state == Torched)
              Cell.hc * (cell.m - mNew) / meteo.dt / 1000.0 // kW/m
            else 0.0

          // Nouvelle cellule (fonctionnelle)
          val nextCell = cell.copy(
            T = Tnew,
            m = mNew,
            mW = new_mW,
            I = intensity,
            state = newState,
            torchedSteps = torchedStepsNew,
            wasTorched = wasTorchedNew
          )

          // --- Génération de brandons (Albini, 1979) ---
          val generatedBrandons =
            if (withBrandons && newState == Torched)
              (1 to 15).flatMap { _ =>
                val (di, dj) = meteo.ventDirection
                val d = math.abs(Random.nextGaussian() * 6).toInt + 1
                val targetI = i + di * d
                val targetJ = j + dj * d
                if (valid(targetI, targetJ, grid)) Some((targetI, targetJ)) else None
              }.toList
            else Nil

          (nextCell, generatedBrandons)
        }
      }

    // Compilation de toutes les coordonnées cible de brandons générés ---
    val brandonsByTarget: Map[(Int, Int), Int] =
      updatedCellsWithBrandons.flatten
        .flatMap(_._2)
        .groupBy(identity)
        .view.mapValues(_.size)
        .toMap

    // Application des brandons (fonctionnel) ---
    val finalCells: List[List[Cell]] =
      updatedCellsWithBrandons.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case ((cell, _), j) =>
          val addedFb = brandonsByTarget.getOrElse((i, j), 0)
          cell.copy(fb = cell.fb + addedFb)
        }
      }

    // Spot fire (Albini, 1979)
    val spotfireCells: List[List[Cell]] =
      finalCells.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (cell, j) =>
          if (
            withSpotFire &&
              cell.fb > 0 &&
              (cell.state == Unburned || cell.state == Heating) &&
              testSpotFire(cell)
          )
            cell.copy(state = Igniting)
          else cell
        }
      }

    Grid(grid.size_X, grid.size_Y, spotfireCells)
  }

  /** Retourne les voisins de Moore d'une cellule */
  def getMooreNeighbors(i: Int, j: Int, grid: Grid): List[Cell] = {
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      if !(dx == 0 && dy == 0)
      ni = i + dx
      nj = j + dy
      if valid(ni, nj, grid)
    } yield grid.cells(ni)(nj)
  }.toList

  /** Vérifie que (i, j) est une coordonnée valide dans la grille */
  def valid(i: Int, j: Int, grid: Grid): Boolean =
    i >= 0 && i < grid.size_X && j >= 0 && j < grid.size_Y

  /**
   * Flux thermiques (Rothermel, 1972; physique générale)
   * - qRad : Rayonnement = ε·σ·Σ(T^4) voisins Burning
   * - qCond : Conduction = λ·Σ(ΔT)
   * - qConv : Convection = h·(Tgaz-Tcell)
   */
  /**
   * Calcule les flux thermiques selon Rothermel (1972), modifiés pour l’effet du vent :
   * - qRad : rayonnement (uniquement voisins Burning)
   * - qCond : conduction (modulée par la direction du vent via windWeight)
   * - qConv : convection (idée simple : dépend du vent et de la diff. de T)
   */
  /**

   */
  def computeFluxes(cell: Cell, neighbors: List[Cell], meteo: Meteo): (Double, Double, Double) = {

    // --- Rayonnement : rayonnement direct des voisins Burning ---
    val qRad = 0.98 * 5.67e-8 * neighbors.filter(_.state == Burning).map(c => math.pow(c.T, 4)).sum

    // --- Conduction : amplification ou blocage selon direction du vent via windWeight ---
    val qCond = neighbors.map { n =>
      val weight = windWeight(cell.i, cell.j, n.i, n.j, meteo)
      weight * (n.T - cell.T)
    }.sum * 4.5 // facteur de conduction de base (ajuste ce coeff pour + ou - de transfert global)

    // --- Convection : uniquement dans l’axe du vent (option simple) ---
    val windAligned = neighbors.filter(n => meteo.isDownwind(cell.i, cell.j, n.i, n.j))
    val Tgaz = if (windAligned.nonEmpty) windAligned.map(_.T).sum / windAligned.size else cell.T
    val qConv = (5.0 + 20.0 * meteo.ventVitesse) * (Tgaz - cell.T)

    (qRad, qCond, qConv)
  }

  /**
   * Poids du transfert selon l’angle entre vent et direction du voisin :
   * - Grand dans le cône sous le vent, très faible contre le vent, normal ailleurs
   * - Le cône favorisé se rétrécit si le vent est fort
   */
  def windWeight(
                  centerI: Int, centerJ: Int, neighborI: Int, neighborJ: Int, meteo: Meteo
                ): Double = {
    val (vi, vj) = meteo.ventDirection
    val ventVitesse = meteo.ventVitesse

    // Vecteur du vent (normalisé)
    val norm = math.sqrt(vi * vi + vj * vj)
    val (windX, windY) = if (norm == 0) (0.0, 0.0) else (vi / norm, vj / norm)

    // Direction du voisin (normalisée)
    val dx = neighborI - centerI
    val dy = neighborJ - centerJ
    val dnorm = math.sqrt(dx * dx + dy * dy)
    val (dirX, dirY) = if (dnorm == 0) (0.0, 0.0) else (dx / dnorm, dy / dnorm)

    // Cosinus de l’angle entre vent et direction du voisin
    val cosAngle = windX * dirX + windY * dirY

    // Seuil dépendant du vent : cône plus étroit si vent fort
    val cosThreshold = 0.7 + 0.3 * math.min(ventVitesse / 10.0, 1.0)

    if (cosAngle >= cosThreshold) {
      // Sous le vent (et diagonale sous le vent)
      7.0 + 3.0 * ventVitesse // Très amplifié
    }
    else if (cosAngle <= -cosThreshold) {
      // Contre le vent (et diagonale contre le vent)
      0.1 // Presque bloqué
    }
    else {
      // Orthogonal ou hors cône principal
      1.0
    }
  }



  // Evaporation de l’humidité du combustible
  def evaporationRate(cell: Cell): Double = cell.mW / 100.0

  /**
   * Critère Van Wagner (1977) : Torching si I >= Io
   * - I : intensité thermique feu de surface (kW/m)
   * - Io : intensité critique (Io = [k·z·Qig/g]^{1.5})
   * - Q_ig = 460 + 2590·mW
   */
  def testTorching(cell: Cell, meteo: Meteo, use: Boolean = false): Boolean = {
    if (use) {
      val ROS = 0.03 // Taux de propagation du feu de surface (m/s)
      val I = Cell.hc * cell.m * ROS
      val Q_ig = 460 + 2590 * cell.mW
      val Io = math.pow(Cell.k * cell.z * Q_ig / 9.81, 1.3)
      I >= Io
    } else false
  }

  /**
   * Critère Sardoy et al. (2008) : Torching si longueur de flamme >= gap sous canopée
   * - Lf = a·I^b (Byram/Sardoy), avec a=0.0775, b=0.46 typiquement
   * - h_gap : gap vertical sous la canopée (m)
   */
  def testSardoyTorching(cell: Cell, use: Boolean = false): Boolean = {
    if (use) {
      val a = 0.0775
      val b = 0.46
      val I = cell.I * 1000 // Conversion kW/m --> W/m si a,b calibrés en W/m
      val Lf = a * math.pow(I, b)
      Lf >= cell.h_gap
    } else false
  }

  /**
   * Déclenchement de spot fire (Albini, 1979)
   * - Modélise la proba qu’un foyer secondaire démarre à distance suite à réception de brandons.
   */
  def testSpotFire(cell: Cell, use: Boolean = false): Boolean = {
    if (use) {
      val P0 = 0.9
      val lambda = 0.2
      val d = 1.0
      val P = 1 - math.pow(1 - P0 * math.exp(-lambda * d), cell.fb)
      Random.nextDouble() < P
    }
    else false
  }
}

