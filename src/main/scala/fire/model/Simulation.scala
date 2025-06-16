package fire.model

class Simulation(val initialGrid: Grid, val meteo: Meteo) {

  /** Retourne un flux paresseux de tous les états successifs de la grille (LazyList) */
  def allSteps(
                withTorching: Boolean = false,
                withBrandons: Boolean = false,
                withSpotFire: Boolean = false,
                withSardoy: Boolean = false
              ): LazyList[Grid] = {

    def loop(current: Grid): LazyList[Grid] =
      current #:: loop(
        Grid.step(
          current, meteo,
          withTorching, withBrandons, withSpotFire, withSardoy
        )
      )

    loop(initialGrid)
  }

  /** Exécute la simulation sur n étapes (en incluant l’état initial) et retourne la liste */
  def runSteps(steps: Int,
               withTorching: Boolean = false,
               withBrandons: Boolean = false,
               withSpotFire: Boolean = false,
               withSardoy: Boolean = false): List[Grid] = {
    allSteps(withTorching, withBrandons, withSpotFire, withSardoy).take(steps + 1).toList
  }

  /** Exécute la simulation jusqu'à ce qu'une condition soit vraie (ex: plus aucun feu) */
  def runUntil(
                stopCondition: Grid => Boolean,
                withTorching: Boolean = false,
                withBrandons: Boolean = false,
                withSpotFire: Boolean = false,
                withSardoy: Boolean = false
              ): List[Grid] = {
    allSteps(withTorching, withBrandons, withSpotFire, withSardoy)
      .takeWhile(g => !stopCondition(g))
      .toList
  }
}
