package fire.model

sealed trait CellStateType
object CellState {
  case object Unburned extends CellStateType
  case object Heating extends CellStateType
  case object Igniting extends CellStateType
  case object Burning extends CellStateType
  case object Torched extends CellStateType
  case object Burned extends CellStateType

  val values: Seq[CellStateType] = Seq(Unburned, Heating, Igniting, Burning, Torched, Burned)
}
