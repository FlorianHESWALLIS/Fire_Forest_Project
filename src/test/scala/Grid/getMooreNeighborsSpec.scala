
package fire.model
import org.scalatest.flatspec.AnyFlatSpec

class getMooreNeighborsSpec extends AnyFlatSpec {
  "getMooreNeighbors" should "donner 8 voisins pour une cellule centrale, moins sur les bords" in {
    val grid = Grid.initializeGrid(3, 3)
    val center = Grid.getMooreNeighbors(1, 1, grid)
    assert(center.size == 8)
    val edge = Grid.getMooreNeighbors(0, 1, grid)
    assert(edge.size == 5)
    val corner = Grid.getMooreNeighbors(0, 0, grid)
    assert(corner.size == 3)
  }


}