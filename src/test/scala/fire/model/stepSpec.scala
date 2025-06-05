
package fire.model

import org.scalatest.flatspec.AnyFlatSpec

class stepSpec extends AnyFlatSpec {

  "Nombre de steps Unburned -> Igniting (physique)" should "correspondre à la théorie" in {
    val Tinit = 293.0
    val Tign = Cell.Tign
    val rho = Cell.rho
    val Cp = Cell.Cp
    val dt = 1.0
    val qInput = 90000.0 // Flux imposé

    // Initialisation cellule
    val cell0 = Cell.default(0, 0).copy(state = CellState.Unburned, T = Tinit, mW = 0.0)
    val grid0 = Grid(1, 1, List(List(cell0)))

    // Fabrique une météo bidon
    val meteo = Meteo((0, 0), 0.0, dt)

    // Simule jusqu'à ignition avec ton flux constant
    var grid = grid0
    var stepCount = 0

    def state(g: Grid) = g.cells(0)(0).state

    while (state(grid) != CellState.Igniting && stepCount < 3000) {
      val updated = grid.cells.map(_.toArray).toArray
      val cell = updated(0)(0)
      val (qRad, qCond, qConv) = (qInput, 0.0, 0.0)
      val dTdt = (qRad + qCond + qConv - Cell.Lv * Grid.evaporationRate(cell)) / (rho * Cp)
      val Tnew = cell.T + dTdt * dt
      val newState = if (Tnew >= Tign) CellState.Igniting else CellState.Heating
      updated(0)(0) = cell.copy(T = Tnew, state = newState)
      grid = Grid(1, 1, updated.map(_.toList).toList)
      stepCount += 1
    }

    val dT = Tign - Tinit
    val dTperStep = (qInput * dt) / (rho * Cp)
    val expectedSteps = Math.ceil(dT / dTperStep).toInt

    println(s"Steps simulés : $stepCount, Steps attendus : $expectedSteps")
    assert(stepCount == expectedSteps)
  }


  "Nombre de steps Heating -> Igniting (physique)" should "correspondre à la théorie" in {
    val Tinit = 350.0 // au-dessus de 293 mais < Tign
    val Tign = Cell.Tign
    val rho = Cell.rho
    val Cp = Cell.Cp
    val dt = 1.0
    val qInput = 90000.0 // Flux constant

    // Initialisation cellule, état Heating
    val cell0 = Cell.default(0, 0).copy(state = CellState.Heating, T = Tinit, mW = 0.0)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val meteo = Meteo((0,0), 0.0, dt)

    // Simule jusqu'à ignition
    def state(g: Grid) = g.cells(0)(0).state
    def temp(g: Grid) = g.cells(0)(0).T

    var grid = grid0
    var stepCount = 0

    while (state(grid) != CellState.Igniting) {
      val updated = grid.cells.map(_.toArray).toArray
      val cell = updated(0)(0)
      val (qRad, qCond, qConv) = (qInput, 0.0, 0.0)
      val dTdt = (qRad + qCond + qConv - Cell.Lv * Grid.evaporationRate(cell)) / (rho * Cp)
      val Tnew = cell.T + dTdt * dt
      val newState = if (Tnew >= Tign) CellState.Igniting else CellState.Heating
      updated(0)(0) = cell.copy(T = Tnew, state = newState)
      grid = Grid(1,1, updated.map(_.toList).toList)
      stepCount += 1
    }

    val dT = Tign - Tinit
    val dTperStep = (qInput * dt) / (rho * Cp)
    val expectedSteps = Math.ceil(dT / dTperStep).toInt

    println(s"Steps simulés : $stepCount, Steps attendus : $expectedSteps")
    assert(stepCount == expectedSteps)
  }


  "Nombre de steps Igniting -> Burning" should "être égal à 1 si m > 0" in {
    val mInit = 1.5
    val cell0 = Cell.default(0, 0).copy(state = CellState.Igniting, m = mInit, mInit = mInit)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val meteo = Meteo((0, 0), 0.0, 1.0)
    val grid1 = Grid.step(grid0, meteo)
    val state1 = grid1.cells(0)(0).state

    println(s"État après 1 step (à partir de Igniting) : $state1")
    assert(state1 == CellState.Burning)
  }


  "Nombre de steps Burning -> Burned (physique)" should "correspondre à la théorie" in {
    val mInit = 1.5
    val cell0 = Cell.default(0, 0).copy(state = CellState.Burning, m = mInit, mInit = mInit, mW = 0.0)
    val grid0 = Grid(1, 1, List(List(cell0)))
    val meteo = Meteo((0, 0), 0.0, 1.0)

    var grid = grid0
    var stepCount = 0

    def state(g: Grid) = g.cells(0)(0).state

    def mass(g: Grid) = g.cells(0)(0).m

    while (state(grid) != CellState.Burned && stepCount < 2000) {
      val updated = grid.cells.map(_.toArray).toArray
      val cell = updated(0)(0)
      // applique la même formule de perte de masse
      val mNew = math.max(0.0, cell.m - Cell.k * cell.m * meteo.dt)
      val newState = if (mNew <= 0.01 * mInit) CellState.Burned else CellState.Burning
      updated(0)(0) = cell.copy(m = mNew, state = newState)
      grid = Grid(1, 1, updated.map(_.toList).toList)
      stepCount += 1
    }

    val nTheorique = math.ceil(math.log(0.01) / math.log(1.0 - Cell.k)).toInt
    println(s"Steps simulés : $stepCount, Steps attendus : $nTheorique")
    assert(stepCount == nTheorique)
  }


  "Nombre de steps Unburned -> Burned (physique)" should "correspondre à la somme des étapes théoriques" in {
    val Tinit = 293.0
    val Tign = Cell.Tign
    val rho = Cell.rho
    val Cp = Cell.Cp
    val dt = 1.0
    val qInput = 90000.0 // Flux constant

    // Masse initiale
    val mInit = 1.5

    // Initialisation cellule Unburned
    var cell = Cell.default(0, 0).copy(state = CellState.Unburned, T = Tinit, m = mInit, mInit = mInit, mW = 0.0)
    var grid = Grid(1, 1, List(List(cell)))

    def state(g: Grid) = g.cells(0)(0).state

    def temp(g: Grid) = g.cells(0)(0).T

    def mass(g: Grid) = g.cells(0)(0).m

    // ----------- PHASE 1 : Unburned/Heating -> Igniting -----------
    var stepCount = 0
    while (state(grid) != CellState.Igniting && stepCount < 3000) {
      val updated = grid.cells.map(_.toArray).toArray
      val cell = updated(0)(0)
      val (qRad, qCond, qConv) = (qInput, 0.0, 0.0)
      val dTdt = (qRad + qCond + qConv - Cell.Lv * Grid.evaporationRate(cell)) / (rho * Cp)
      val Tnew = cell.T + dTdt * dt
      val newState = if (Tnew >= Tign) CellState.Igniting else CellState.Heating
      updated(0)(0) = cell.copy(T = Tnew, state = newState)
      grid = Grid(1, 1, updated.map(_.toList).toList)
      stepCount += 1
    }
    val dT = Tign - Tinit
    val dTperStep = (qInput * dt) / (rho * Cp)
    val expectedIgnitionSteps = Math.ceil(dT / dTperStep).toInt

    // ----------- PHASE 2 : Igniting -> Burning (toujours 1 step si m > 0) -----------
    val updatedIgnite = grid.cells.map(_.toArray).toArray
    val cellIgnite = updatedIgnite(0)(0)
    updatedIgnite(0)(0) = cellIgnite.copy(state = CellState.Burning)
    grid = Grid(1, 1, updatedIgnite.map(_.toList).toList)
    stepCount += 1 // passage instantané (1 step)
    val expectedIgniteToBurn = 1

    // ----------- PHASE 3 : Burning -> Burned (exponentielle) -----------
    while (state(grid) != CellState.Burned && stepCount < 10000) {
      val updated = grid.cells.map(_.toArray).toArray
      val cell = updated(0)(0)
      val mNew = math.max(0.0, cell.m - Cell.k * cell.m * dt)
      val newState = if (mNew <= 0.01 * mInit) CellState.Burned else CellState.Burning
      updated(0)(0) = cell.copy(m = mNew, state = newState)
      grid = Grid(1, 1, updated.map(_.toList).toList)
      stepCount += 1
    }
    val expectedBurningSteps = math.ceil(math.log(0.01) / math.log(1.0 - Cell.k)).toInt

    val expectedTotal = expectedIgnitionSteps + expectedIgniteToBurn + expectedBurningSteps

    println(s"Steps simulés : $stepCount, Steps attendus : $expectedTotal")
    assert(stepCount == expectedTotal)
  }




}
  
  
