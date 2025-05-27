package fire.model

case class Meteo(
                  ventDirection: (Int, Int), // direction préférée (ex : (0, -1) pour nord)
                  ventVitesse: Double,
                  dt: Double // pas de temps en secondes
                ) {
  def isUpwind(fromI: Int, fromJ: Int, toI: Int, toJ: Int): Boolean = {
    val (di, dj) = (toI - fromI, toJ - fromJ)
    (di, dj) == ventDirection
  }
}
