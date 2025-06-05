package fire.model
import org.scalatest.flatspec.AnyFlatSpec

class MeteoSpec extends AnyFlatSpec {
  "isUpwind" should "reconnaître la direction correcte" in {
    val meteo = Meteo((1, -1), ventVitesse = 1.0, dt = 1.0)

    // Test : du voisin (0,2) vers le centre (1,1) => (1,1)-(0,2) = (1,-1) == ventDirection
    assert(meteo.isUpwind(0, 2, 1, 1))  // doit être true

    // Test : du voisin (1,0) vers le centre (1,1) => (1,1)-(1,0) = (0,1)
    assert(!meteo.isUpwind(1, 0, 1, 1)) // doit être false si ventDirection = (1,-1)

    // Test : du voisin (2,2) vers le centre (1,1) => (1,1)-(2,2) = (-1,-1)
    assert(!meteo.isUpwind(2, 2, 1, 1))

    // Test exact inverse
    val meteo2 = Meteo((0,1), ventVitesse=1.0, dt=1.0)
    assert(meteo2.isUpwind(1,0,1,1)) // (1,1)-(1,0) = (0,1)
    assert(!meteo2.isUpwind(0,1,1,1))
  }
}
