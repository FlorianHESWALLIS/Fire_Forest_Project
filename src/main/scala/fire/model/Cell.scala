package fire.model

import fire.model.CellState._
import fire.model.CellStateType

case class Cell(
                 i: Int,                 // Coordonnée ligne (indice vertical de la grille)
                 j: Int,                 // Coordonnée colonne (indice horizontal de la grille)
                 state: CellStateType,   // État logique (Unburned, Heating, etc.)
                 T: Double,              // Température locale (K)
                 m: Double,              // Masse actuelle de combustible sec (kg/m²)
                 mInit: Double,          // Masse initiale de combustible (kg/m²)
                 mW: Double,             // Humidité relative (kg eau / kg sec)
                 I: Double,              // Intensité thermique (kW/m)
                 fb: Int                 // Nombre de brandons reçus
               )

object Cell {
  def default(i: Int, j: Int): Cell =
    Cell(
      i = i,
      j = j,
      state = Unburned,
      T = 293.0,    // 20 °C
      m = 1.5,      // kg/m²
      mInit = 1.5,
      mW = 0.15,    // 15 % humidité
      I = 0.0,
      fb = 0
    )

  // Constantes physiques associées au combustible et au milieu

  val Tign = 600.0 // Température d’ignition du combustible (K) — seuil à partir duquel la combustion démarre

  val Cp = 1800.0 // Capacité thermique massique du combustible (J/kg/K) — quantité d’énergie nécessaire pour élever de 1K 1kg de matière

  val hc = 18000.0 // Chaleur de combustion du combustible (J/kg) — énergie dégagée par kg de matière brûlée

  val rho = 350.0 // Densité apparente du combustible (kg/m³) — utilisée pour les bilans thermiques

  val k = 0.01 // Constante de cinétique de combustion (s⁻¹) — rythme de disparition de la masse combustible

  val Lv = 2.5e6 // Chaleur latente de vaporisation de l’eau (J/kg) — énergie nécessaire pour évaporer l’humidité présente dans la cellule
  
}



