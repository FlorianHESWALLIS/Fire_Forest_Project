package fire.model

import org.scalatest.flatspec.AnyFlatSpec
class StatesSpec extends AnyFlatSpec {

  val mInit = 1.0
  val meteo = Meteo((0, 0), 0.0, 10.0) // dt choisi assez grand pour accélérer

  "Unburned -> Heating" should "chauffer si T < Tign" in {
    val cell0 = Cell(0, 0, CellState.Unburned, T = 400, m = mInit, mInit = mInit, mW = 0, I = 0, fb = 0, z = 5)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val grid1 = Grid.step(grid0, meteo, withBrandons = false, withSpotFire = false)
    val c1 = grid1.cells(0)(0)
    println(s"Unburned→Heating : état=${c1.state}, T=${c1.T}, m=${c1.m}")
    assert(c1.state == CellState.Heating)
  }

  "Unburned -> Igniting" should "s'enflammer si T >= Tign" in {
    val cell0 = Cell(0, 0, CellState.Unburned, T = Cell.Tign + 1, m = mInit, mInit = mInit, mW = 0, I = 0, fb = 0, z = 5)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val grid1 = Grid.step(grid0, meteo, withBrandons = false, withSpotFire = false)
    val c1 = grid1.cells(0)(0)
    println(s"Unburned→Igniting : état=${c1.state}, T=${c1.T}, m=${c1.m}")
    assert(c1.state == CellState.Igniting)
  }

  "Heating -> Igniting" should "passe à Igniting si T >= Tign" in {
    val cell0 = Cell(0, 0, CellState.Heating, T = Cell.Tign + 10, m = mInit, mInit = mInit, mW = 0, I = 0, fb = 0, z = 5)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val grid1 = Grid.step(grid0, meteo, withBrandons = false, withSpotFire = false)
    val c1 = grid1.cells(0)(0)
    println(s"Heating→Igniting : état=${c1.state}, T=${c1.T}, m=${c1.m}")
    assert(c1.state == CellState.Igniting)
  }

  "Igniting -> Burning" should "si m > 0" in {
    val cell0 = Cell(0, 0, CellState.Igniting, T = 700, m = mInit, mInit = mInit, mW = 0, I = 0, fb = 0, z = 5)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val grid1 = Grid.step(grid0, meteo, withBrandons = false, withSpotFire = false)
    val c1 = grid1.cells(0)(0)
    println(s"Igniting→Burning : état=${c1.state}, T=${c1.T}, m=${c1.m}")
    assert(c1.state == CellState.Burning)
  }

  "Igniting -> Burned" should "si m == 0" in {
    val cell0 = Cell(0, 0, CellState.Igniting, T = 700, m = 0, mInit = mInit, mW = 0, I = 0, fb = 0, z = 5)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val grid1 = Grid.step(grid0, meteo, withBrandons = false, withSpotFire = false)
    val c1 = grid1.cells(0)(0)
    println(s"Igniting→Burned : état=${c1.state}, T=${c1.T}, m=${c1.m}")
    assert(c1.state == CellState.Burned)
  }

  "Burning -> Burned" should "quand la masse passe sous le seuil" in {
    val m_seuil = 0.009 // < 0.01
    val cell0 = Cell(0, 0, CellState.Burning, T = 700, m = m_seuil, mInit = mInit, mW = 0, I = 0, fb = 0, z = 5)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val grid1 = Grid.step(grid0, meteo, withBrandons = false, withSpotFire = false)
    val c1 = grid1.cells(0)(0)
    println(s"Burning→Burned : état=${c1.state}, T=${c1.T}, m=${c1.m}")
    assert(c1.state == CellState.Burned)
  }

  // Tester de Burning à Torched
  "Burning -> Torched" should "se produire si les conditions sont favorables" in {
    val mInit = 10.0 // Masse très grande
    val cell0 = Cell(0, 0, CellState.Burning, T = 1000, m = mInit, mInit = mInit, mW = 0.0, I = 0, fb = 0, z = 1.0) // canopée basse
    val meteo = Meteo((0, 0), 0.0, 10.0)
    var grid = Grid(1, 1, List(List(cell0)))

    // On tente jusqu'à réussite (probabilité proche de 1 avec ces paramètres)
    var found = false
    for (_ <- 0 until 10 if !found) {
      grid = Grid.step(grid, meteo, withBrandons = false, withSpotFire = false)
      val etat = grid.cells(0)(0).state
      println(s"état après step: $etat (m = ${grid.cells(0)(0).m})")
      if (etat == CellState.Torched) found = true
    }
    assert(found, "La cellule devrait être passée Torched dans ces conditions.")
  }

  "Spot fire" should "mettre la cellule à Igniting si fb élevé" in {
    val mInit = 1.0
    val fbHigh = 100 // Un nombre très élevé de brandons pour forcer la proba
    val cell0 = Cell(0, 0, CellState.Unburned, T = 300, m = mInit, mInit = mInit, mW = 0.0, I = 0, fb = fbHigh, z = 5)
    val meteo = Meteo((0, 0), 0.0, 1.0)
    var grid = Grid(1, 1, List(List(cell0)))

    // On fait jusqu'à 5 steps pour être (presque) certain
    var ignited = false
    for (_ <- 0 until 5 if !ignited) {
      grid = Grid.step(grid, meteo, withBrandons = false, withSpotFire = true)
      val etat = grid.cells(0)(0).state
      println(s"état après step (fb=$fbHigh): $etat")
      if (etat == CellState.Igniting) ignited = true
    }
    assert(ignited, "La cellule aurait dû passer à Igniting avec autant de brandons !")
  }







} 
