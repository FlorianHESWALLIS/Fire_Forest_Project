package fire.model

import fire.model.CellState._

/**
 * Représente une cellule de la grille :
 * - i, j : coordonnées
 * - state : état logique
 * - T : température locale (K)
 * - m : masse actuelle de combustible sec (kg/m²)
 * - mInit : masse initiale de combustible (kg/m²)
 * - mW : humidité relative (kg eau / kg sec)
 * - I : intensité thermique (kW/m)
 * - fb : nombre de brandons reçus
 * - z : hauteur de la canopée (m)
 * - h_gap : gap vertical sous la canopée (m)
 * - torchedSteps : nombre d'étapes restantes en mode Torched
 * - wasTorched : True si déjà passé en Torched (empêche double embrasement)
 */
case class Cell(
                 i: Int,
                 j: Int,
                 state: CellStateType,
                 T: Double,
                 m: Double,
                 mInit: Double,
                 mW: Double,
                 I: Double,
                 fb: Int,
                 z: Double,
                 h_gap: Double,
                 torchedSteps: Int,
                 wasTorched: Boolean,
                 isFront: Boolean
               )

object Cell {
  // Constantes physiques
  val Tign = 600.0 // Température d’ignition (K)
  val Cp = 1800.0 // Capacité thermique massique (J/kg/K)
  val hc = 18000.0 // Chaleur de combustion (J/kg)
  val rho = 350.0 // Densité apparente du combustible (kg/m³)
  val k = 0.01 // Constante de combustion (s⁻¹)
  val Lv = 2.5e6 // Chaleur latente de vaporisation (J/kg)

  /**
   * Cellule par défaut (unburned, combustible standard, humidité moyenne)
   */
  def default(i: Int, j: Int): Cell = Cell(
    i = i,
    j = j,
    state = Unburned,
    T = 293.0, // 20°C
    m = 2.8, // kg/m² — combustible élevé (litière d’aiguilles + branchages)
    mInit = 2.8,
    mW = 0.06, // kg eau/kg sec — forêt de résineux sèche
    I = 0.0,
    fb = 0,
    z = 18.0, // m — canopée de sapin adulte
    h_gap = 4.0, // m — sous-bois dégagé
    torchedSteps = 0,
    wasTorched = false,
    isFront = false
  )
}
