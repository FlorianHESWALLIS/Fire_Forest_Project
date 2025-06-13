package fire.model

import org.scalatest.flatspec.AnyFlatSpec

class StepTransitionsSpec extends AnyFlatSpec {

  // À tester : T_Tree1 = 1850, 2850...
  val T_TESTS = Seq(1850.0, 2850.0)

  for (T_Tree1 <- T_TESTS) {
    s"Une cellule Burning à $T_Tree1 K voisine d'une Unburned" should "passer par toutes les transitions attendues, avec le bon nombre de steps" in {
      val dt = 1.0 // Pas de temps (en secondes)
      val mInit = 1.5 // Masse initiale de combustible (kg)

      // --- PHASE 1: Unburned -> Igniting (via Heating) ---
      val qRad = 0.9 * 5.67e-8 * Math.pow(T_Tree1, 4) // Loi de Stefan-Boltzmann
      val dTperStep = qRad / (Cell.rho * Cell.Cp)
      val nSteps_Unburned_to_Igniting = math.ceil((Cell.Tign - 293.0) / dTperStep).toInt

      // --- PHASE 2: Igniting -> Burning ---
      val nSteps_Igniting_to_Burning = 1

      // --- PHASE 3: Burning -> Burned ---
      val k = Cell.k
      val nSteps_Burning_to_Burned = math.ceil(math.log(0.01) / math.log(1 - k)).toInt

      // --- TOTAL ---
      val nStepsTheory = nSteps_Unburned_to_Igniting + nSteps_Igniting_to_Burning + nSteps_Burning_to_Burned

      println(s"\n------ [TEST pour T_Tree1 = $T_Tree1] ------")
      println(s"Phase 1 (Unburned->Igniting) : $nSteps_Unburned_to_Igniting steps")
      println(s"Phase 2 (Igniting->Burning)   : $nSteps_Igniting_to_Burning step")
      println(s"Phase 3 (Burning->Burned)     : $nSteps_Burning_to_Burned steps")
      println(s"Nombre de pas théorique Unburned → Burned : $nStepsTheory")

      // --- Initialisation de la grille ---
      val burningCell = Cell.default(0, 0).copy(state = CellState.Burning, T = T_Tree1, m = mInit, mInit = mInit, mW = 0.0)
      val unburnedCell = Cell.default(0, 1).copy(state = CellState.Unburned, T = 293.0, m = mInit, mInit = mInit, mW = 0.0)
      var grid = Grid(1, 2, List(List(burningCell, unburnedCell)))
      val meteo = Meteo((0, 0), 0.0, dt)

      // Historique états (avant et après chaque step !)
      val statesHistory = scala.collection.mutable.ArrayBuffer.empty[CellStateType]
      val statesTransitions = scala.collection.mutable.ArrayBuffer.empty[(CellStateType, CellStateType)]

      def rightCellState = grid.cells(0)(1).state

      var nStepsSimu = 0
      var prevState = rightCellState

      while (rightCellState != CellState.Burned && nStepsSimu < nStepsTheory * 2) {
        statesHistory += rightCellState             // état avant step
        grid = Grid.step(grid, meteo)
        val newState = rightCellState
        statesTransitions += ((prevState, newState))// enregistre la transition
        prevState = newState
        nStepsSimu += 1
      }
      statesHistory += rightCellState // Ajoute le dernier état

      println(s"Nombre de steps simulés: $nStepsSimu, théorique: $nStepsTheory")
      // println("États successifs cellule droite: " + statesHistory.mkString(" → "))

      // ---- Vérification transitions attendues ----
      // Unburned doit être vu
      assert(statesHistory.contains(CellState.Unburned), "Devrait commencer à Unburned")
      // Heating doit être vu
      assert(statesHistory.contains(CellState.Heating), "Doit passer par Heating")
      // Vérifie la transition Heating → Igniting (peut être très brève, donc on teste la transition et non la présence d'Igniting dans la liste)
      assert(
        statesTransitions.exists { case (from, to) => from == CellState.Heating && to == CellState.Igniting },
        "Doit passer de Heating à Igniting"
      )
      // Idem pour Igniting → Burning
      assert(
        statesTransitions.exists { case (from, to) => from == CellState.Igniting && to == CellState.Burning },
        "Doit passer de Igniting à Burning"
      )
      // Doit finir Burned
      assert(statesHistory.last == CellState.Burned, "Doit finir Burned")
      // Steps total tolérance ±1
      assert(math.abs(nStepsSimu - nStepsTheory) <= 1, s"Le nombre de steps simulé ($nStepsSimu) doit être proche de la théorie ($nStepsTheory)")
    }
  }
}
