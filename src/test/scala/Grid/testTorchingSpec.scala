
package fire.model

import org.scalatest.flatspec.AnyFlatSpec

class testTorchingSpec extends AnyFlatSpec {
  "testTorching" should "donner une proba très élevée pour des conditions extrêmes" in {
    val cell = Cell.default(0, 0).copy(m = 10.0, mW = 0.0, z = 20.0)
    val meteo = Meteo((0, 1), 1.0, 1.0)
    // Si la proba est proche de 1, sur 100 essais on doit avoir quasiment toujours true
    val n = 100
    val results = (1 to n).map(_ => Grid.testTorching(cell, meteo))
    val count = results.count(identity)
    assert(count > n * 0.95) // au moins 95%
  }

}
  
  
  
  

