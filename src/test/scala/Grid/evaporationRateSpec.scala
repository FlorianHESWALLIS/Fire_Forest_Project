
package fire.model
import org.scalatest.flatspec.AnyFlatSpec

class evaporationRateSpec extends AnyFlatSpec {
  "evaporationRate" should "être proportionnelle à l'humidité" in {
    val c1 = Cell.default(0, 0).copy(mW = 0.0)
    val c2 = Cell.default(0, 0).copy(mW = 0.5)
    val c3 = Cell.default(0, 0).copy(mW = 1.0)
    assert(Grid.evaporationRate(c1) == 0.0)
    assert(Grid.evaporationRate(c2) == 0.005)
    assert(Grid.evaporationRate(c3) == 0.01)
  }


}