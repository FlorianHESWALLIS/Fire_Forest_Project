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
   * Simule une étape sur toute la grille :


   * - Rothermel (1972) pour l’évolution de la température, masse, intensité.
  
   */
  def step(
            grid: Grid,
            meteo: Meteo,
          ): Grid = {


    val t_MAX = 2000.0 // Température max (K)

    // Génération des nouvelles cellules
    val updatedCells: List[List[Cell]] =
      grid.cells.zipWithIndex.map { case (row, i) =>
        row.zipWithIndex.map { case (cell, j) =>
          val neighbors = getMooreNeighbors(i, j, grid)

          // --- Flux thermiques (Rothermel, 1972) ---
          val (qRad, qCond, qConv) = computeFluxes(cell, neighbors, meteo)
          val dTdt = (qRad + qCond + qConv - Cell.Lv * evaporationRate(cell)) / (Cell.rho * Cell.Cp)
          val Tnew = math.min(cell.T + 5 * dTdt * meteo.dt, t_MAX)

          // Transition d'état
          val newState = cell.state match {
            case Burning =>
              if (cell.m <= 0.0001 * cell.mInit) Burned
              else Burning
            case Unburned | Heating =>
              if (Tnew >= Cell.Tign) Igniting
              else Heating
            case Igniting =>
              if (cell.m > 0) Burning
              else Burned
            case Burned => Burned
          }

          // --- Évolution de la masse de combustible (Rothermel, 1972) ---

          // --- PARAMÈTRE EMPIRIQUE ---
          // Influence du vent sur la combustion locale (0.0 = aucune influence, 0.05 = influence notable)
          val alpha: Double = 0.05

          val mNew =
            if (cell.state == Burning)
              math.max(0.0, cell.m - Cell.k * (1.0 + alpha * meteo.ventVitesse) * cell.m * meteo.dt)
            else cell.m

          val evaporation = evaporationRate(cell) * meteo.dt
          val new_mW = math.max(0.0, cell.mW - evaporation)

          // Nouvelle cellule
          cell.copy(
            T = Tnew,
            m = mNew,
            mW = new_mW,
            state = newState
          )
        }
      }

    Grid(grid.size_X, grid.size_Y, updatedCells)
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
   *
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
}
 
  
