
package fire.model

import org.scalatest.flatspec.AnyFlatSpec

class MainSpec extends AnyFlatSpec {



  "La cellule centrale d'une grille 3x3" should "avoir exactement 8 voisins" in {
    val grid = Grid.initializeGrid(3, 3)
    val neighbors = Grid.getMooreNeighbors(1, 1, grid)
    // Debug (optionnel) :
    println("Coords des voisins du centre :")
    neighbors.foreach(c => println(s"(${c.i},${c.j})"))
    assert(neighbors.size == 8)
    // On peut vérifier aussi qu'aucun voisin n'est hors grille
    assert(neighbors.forall(c => c.i >= 0 && c.i < 3 && c.j >= 0 && c.j < 3))
    // Et que toutes les positions attendues sont là
    val expectedCoords = Set(
      (0, 0), (0, 1), (0, 2),
      (1, 0), (1, 2),
      (2, 0), (2, 1), (2, 2)
    )
    val neighborCoords = neighbors.map(c => (c.i, c.j)).toSet
    assert(neighborCoords == expectedCoords)
  }





}