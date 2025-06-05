
package fire.model

import org.scalatest.flatspec.AnyFlatSpec

class testSpotFireSpec extends AnyFlatSpec {
  "testSpotFire" should "donner une proba quasi-nulle si fb=0, quasi 1 si fb élevé" in {
    val c0 = Cell.default(0, 0).copy(fb = 0)
    val c5 = Cell.default(0, 0).copy(fb = 20)
    val n = 100
    val res0 = (1 to n).count(_ => Grid.testSpotFire(c0))
    val res5 = (1 to n).count(_ => Grid.testSpotFire(c5))
    assert(res0 < n * 0.1, s"Probabilité trop haute pour fb=0, obtenu: $res0")
    assert(res5 > n * 0.9, s"Probabilité trop basse pour fb=20, obtenu: $res5")
  }


}