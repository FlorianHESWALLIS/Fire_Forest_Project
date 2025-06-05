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
                 fb: Int,                // Nombre de brandons reçus
                 z: Double               // Hauteur de base de la canopée (m)
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
      fb = 0,
      z = 5.0       // hauteur moyenne de la canopée (par défaut 5 mètres)
    )


  // Constantes physiques associées au combustible et au milieu

  val Tign = 600.0    // Température d’ignition du combustible (K)
  val Cp   = 1800.0   // Capacité thermique massique (J/kg/K)
  val hc   = 18000.0  // Chaleur de combustion (J/kg)
  val rho  = 350.0    // Densité apparente du combustible (kg/m³)
  val k    = 0.01     // Constante de combustion (s⁻¹)
  val Lv   = 2.5e6    // Chaleur latente de vaporisation (J/kg)
}
