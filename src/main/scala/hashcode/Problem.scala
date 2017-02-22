package hashcode

case class Problem(pizza: Vector[String], nrow: Int, ncol: Int, minIngredients: Int, maxCells: Int, rowShit: Int = 0, colShift: Int = 0, name: String = "problem") {
  def ingredientAt(r: Int, c: Int): Char = pizza(r)(c)

  def splitInSmallerProblems: Iterable[Problem] = {
    for {
      startRow <- 0 until nrow by maxCells
      startCol <- 0 until ncol by maxCells
      zone = pizza.drop(startRow).take(maxCells).map(line => line.drop(startCol).take(maxCells))
    } yield Problem(zone, zone.size, zone.head.size, minIngredients, maxCells, startRow, startCol)
  }

  lazy val _validSlices: Vector[Slice] = {
    for {
      startRow <- (0 until nrow).par
      startCol <- 0 until ncol
      slice <- slicesStartingAt(startRow, startCol)
      if Validator.checkSlice(slice, this).isEmpty
    } yield slice
  }.toVector.sortBy(-_.size)

  lazy val validSlices: List[Slice] = _validSlices.toList.sortBy(sliceIntersections)

  lazy val sliceIntersections: Map[Slice, Int] =
    _validSlices.map { s1 =>
      val count = _validSlices.filterNot(_ == s1).count(_.intersects(s1))
      s1 -> count
    }.toMap

  def slicesStartingAt(startRow: Int, startCol: Int): Iterable[Slice] = {
    val slices = for {
      endRow <- startRow + 1 to (nrow min (startRow + maxCells))
      rows = endRow - startRow
      endCol <- startCol + 1 to (ncol min (startCol + maxCells / rows))
      cols = endCol - startCol
      if rows * cols <= maxCells
      slice = Slice(startRow, endRow, startCol, endCol)
    } yield slice
    slices
  }
}
