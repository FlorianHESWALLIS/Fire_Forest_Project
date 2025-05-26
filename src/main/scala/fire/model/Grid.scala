

case class Grid(cells: Vector[Vector[Cell]]):
  val nRows = cells.length
  val nCols = cells(0).length