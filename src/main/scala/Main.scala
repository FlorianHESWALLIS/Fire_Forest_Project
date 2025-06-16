package fire

import fire.model.*

import scala.annotation.tailrec

object Main extends App {

  // 1. Créer une grille 1×5
  val grid0 = Grid.initializeGrid(1, 1)

  // 2. Modifier la hauteur de canopée (z) selon position
  val updatedCells = grid0.cells.zipWithIndex.map { case (row, i) =>
    row.zipWithIndex.map { case (cell, j) =>
      val z =  10 // forêt moyenne
      cell.copy(z = z)
    }
  }
  val grid1 = grid0.copy(cells = updatedCells)

  val x = 0
  val y = 0


  // 3. Allumer la cellule centrale (ligne 0, colonne 2)
  val centerCell = grid1.cells(x)(y).copy(state = CellState.Burning, T = 850.0)
  val grid2 = grid1.copy(
    cells = grid1.cells.updated(x, grid1.cells(x).updated(y, centerCell))
  )

  // 4. Météo (⚠️ dt > 0 sinon aucune évolution !)
  val meteo = Meteo(ventDirection = (0, 0), ventVitesse = 0.0, dt = 1)

  // 5. Afficher grille avant propagation
  println("🧪 AVANT propagation")
  printGrid(grid2, showZ = false)

  // 6. Simuler la propagation du feu (exemple 10 pas)
  val gridFinal = simulate_fire(grid2, 3000)

  // 7. Afficher grille après propagation
  println("\n🔥 APRÈS propagation")
  printGrid(gridFinal, showZ = false)

  // --- Méthodes utilitaires ---

  // Affichage grille
  def printGrid(grid: Grid, showZ: Boolean): Unit = {
    for (row <- grid.cells) {
      println(row.map { cell =>
        if (showZ) f"${cell.z}%.1f"
        else stateSymbol(cell.state)
      }.mkString(" "))
    }
  }

  // Symboles Unicode pour afficher les états
  def stateSymbol(state: CellStateType): String = state match {
    case CellState.Unburned => "U"//"🟩"
    case CellState.Heating  => "H"//"🟨"
    case CellState.Igniting => "I"//"🟧"
    case CellState.Burning  => "B"//"🟥"
    case CellState.Torched  => "🔥"
    case CellState.Burned   => "⬛"
  }

  // Simulation fonctionnelle récursive
  @tailrec
  def simulate_fire(grid: Grid, steps: Int): Grid = {
    println(f"\n🔥 step : $steps propagation")
    printGrid(grid, showZ = false)
    if (steps == 0) grid
    else simulate_fire(Grid.step(grid, meteo), steps - 1)
  }

  @tailrec
  def simulate_fire_until_burned(
                                  grid: Grid,
                                  meteo: Meteo,
                                  step: Int = 0,
                                  withBrandons: Boolean = false,
                                  withSpotFire: Boolean = false
                                ): (Grid, Int) = {
    // Arrêt si toutes les cellules sont "Burned"
    if (grid.cells.flatten.forall(_.state == CellState.Burned)) (grid, step)
    else {
      val gridNext = Grid.step(grid, meteo, withBrandons, withSpotFire)
      simulate_fire_until_burned(gridNext, meteo, step + 1, withBrandons, withSpotFire)
    }
  }

  val (finalGrid, steps) = simulate_fire_until_burned(grid2, meteo)
  println(s"Simulation terminée après $steps étapes.")

}
