package fire.model

import fire.model.CellState._
import scala.util.Random

case class Grid(size_X: Int, size_Y: Int, cells: List[List[Cell]])

object Grid {

  def initializeGrid(size_X: Int, size_Y: Int): Grid = {
    val cells = (0 until size_X).toList.map { i =>
      (0 until size_Y).toList.map { j => Cell.default(i, j) }
    }
    Grid(size_X, size_Y, cells)
  }

  def step(grid: Grid, meteo: Meteo): Grid = {
    val updated = grid.cells.map(_.toArray).toArray

    val brandons = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]

    for {
      i <- 0 until grid.size_X
      j <- 0 until grid.size_Y
    } {
      val cell = updated(i)(j)
      val neighbors = getMooreNeighbors(i, j, grid)

      val (qRad, qCond, qConv) = computeFluxes(cell, neighbors, meteo)
      val dTdt = (qRad + qCond + qConv - Cell.Lv * evaporationRate(cell)) / (Cell.rho * Cell.Cp)
      val Tnew = cell.T + dTdt * meteo.dt

      val newState = cell.state match {
        case Unburned | Heating =>
          if (Tnew >= Cell.Tign) Igniting else Heating
        case Igniting =>
          if (cell.m > 0) Burning else Burned
        case Burning =>
          if (cell.m <= 0.01 * cell.mInit) Burned
          else if (testTorching(cell, meteo)) Torched else Burning
        case Torched | Burned => cell.state
      }

      val mNew = if (cell.state == Burning) math.max(0.0, cell.m - Cell.k * cell.m * meteo.dt) else cell.m

      updated(i)(j) = cell.copy(T = Tnew, m = mNew, state = newState)

      if (newState == Torched) {
        for (_ <- 0 until 5) {
          val (di, dj) = meteo.ventDirection
          val d = math.abs(Random.nextGaussian() * 3).toInt + 1
          val targetI = i + di * d
          val targetJ = j + dj * d
          if (valid(targetI, targetJ, grid)) {
            brandons.append((targetI, targetJ))
          }
        }
      }
    }

    // Affectation des brandons
    brandons.groupBy(identity).view.mapValues(_.size).foreach {
      case ((bi, bj), count) =>
        val oldCell = updated(bi)(bj)
        updated(bi)(bj) = oldCell.copy(fb = oldCell.fb + count)
    }

    // Spot fire
    for {
      i <- 0 until grid.size_X
      j <- 0 until grid.size_Y
      cell = updated(i)(j)
      if cell.fb > 0 && (cell.state == Unburned || cell.state == Heating)
    } {
      if (testSpotFire(cell)) {
        updated(i)(j) = cell.copy(state = Igniting)
      }
    }

    Grid(grid.size_X, grid.size_Y, updated.map(_.toList).toList)
  }

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

  def valid(i: Int, j: Int, grid: Grid): Boolean =
    i >= 0 && i < grid.size_X && j >= 0 && j < grid.size_Y

  def computeFluxes(cell: Cell, neighbors: List[Cell], meteo: Meteo): (Double, Double, Double) = {
    val qRad = 0.9 * 5.67e-8 * neighbors.filter(_.state == Burning).map(c => math.pow(c.T, 4)).sum
    val qCond = 2.0 * neighbors.map(n => n.T - cell.T).sum
    val windAligned = neighbors.filter(n => meteo.isUpwind(n.i, n.j, cell.i, cell.j))
    val Tgaz = if (windAligned.nonEmpty) windAligned.map(_.T).sum / windAligned.size else cell.T
    val qConv = 15.0 * (Tgaz - cell.T)
    (qRad, qCond, qConv)
  }

  def evaporationRate(cell: Cell): Double = cell.mW / 100.0

  def testTorching(cell: Cell, meteo: Meteo): Boolean = {
    val ROS = 0.03
    val I = Cell.hc * cell.m * ROS
    val Q_ig = 460 + 2590 * cell.mW
    val Io = math.pow(Cell.k * cell.z * Q_ig / 9.81, 1.5)
    val p = 1.0 / (1.0 + math.exp(-0.01 * (I - Io)))
    Random.nextDouble() < p
  }

  def testSpotFire(cell: Cell): Boolean = {
    val P0 = 0.6
    val lambda = 0.5
    val d = 1.0
    val P = 1 - math.pow(1 - P0 * math.exp(-lambda * d), cell.fb)
    Random.nextDouble() < P
  }
}
