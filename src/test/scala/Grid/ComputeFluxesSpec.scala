
package fire.model

import org.scalatest.flatspec.AnyFlatSpec

class ComputeFluxesSpec extends AnyFlatSpec {
  val mInit = 1.0
  val meteo = Meteo((0, 0), 0.0, 10.0) // dt choisi assez grand pour accélérer

  "computeFluxes" should "avoir qConv == 0 si aucun voisin upwind" in {
    val cell = Cell(0, 0, CellState.Heating, T = 400, m = 1, mInit = 1, mW = 0, I = 0, fb = 0, z = 5)
    val neighbors = List.fill(3)(cell.copy(state = CellState.Burning, T = 800)) ++ List.fill(5)(cell.copy(state = CellState.Heating, T = 400))
    val meteo = Meteo((2, 2), 0.0, 1.0) // direction impossible, aucun voisin aligné
    val (qRad, qCond, qConv) = Grid.computeFluxes(cell, neighbors, meteo)
    println(f"[VENT EXOTIQUE] qConv = $qConv (attendu : 0)")
    assert(qConv == 0.0)
  }

  "computeFluxes" should "donner un flux convectif uniquement pour la direction du vent pointant le bon voisin" in {
    val cell = Cell(1, 1, CellState.Heating, T = 400, m = 1, mInit = 1, mW = 0, I = 0, fb = 0, z = 5)
    val directions = List(
      ((-1, -1), (0, 0)), // haut-gauche
      ((-1, 0), (0, 1)), // haut
      ((-1, 1), (0, 2)), // haut-droite
      ((0, -1), (1, 0)), // gauche
      ((0, 1), (1, 2)), // droite
      ((1, -1), (2, 0)), // bas-gauche
      ((1, 0), (2, 1)), // bas
      ((1, 1), (2, 2)) // bas-droite
    )
    // Prépare une grille où chaque voisin a 400, sauf un à 800
    for (((_ /*dirInutilisé*/ , neighborCoord), idx) <- directions.zipWithIndex) {
      // Tous les voisins à 400
      val cells = Array.tabulate(3, 3)((i, j) => cell.copy(i = i, j = j, T = 400))
      // Place le voisin d'intérêt à 800
      val (ni, nj) = neighborCoord
      cells(ni)(nj) = cells(ni)(nj).copy(T = 800)
      val grid2 = Grid(3, 3, cells.map(_.toList).toList)
      val neighbors = Grid.getMooreNeighbors(1, 1, grid2)
      // La direction du vent doit être (cellule_centrale - voisin) !
      val ventDir = (1 - ni, 1 - nj)
      val meteo = Meteo(ventDir, 1.0, 1.0)
      val (_, _, qConv) = Grid.computeFluxes(cell, neighbors, meteo)
      println(f"Vent $ventDir%-7s, voisin $neighborCoord, qConv = $qConv%.1f")
      assert(qConv == 15 * (800 - 400), s"qConv devrait être 6000 quand vent vers $ventDir, voisin $neighborCoord")

      // Si le vent ne pointe pas vers ce voisin : aucun flux convectif attendu
      val meteoNo = Meteo((9, 9), 1.0, 1.0) // direction impossible
      val (_, _, qConvNo) = Grid.computeFluxes(cell, neighbors, meteoNo)
      assert(qConvNo == 0.0, s"qConv devrait être 0 quand vent direction impossible")
    }
  }
  
  
  
  
  
  
  
  
  
  
}
