package hashcode

case class Problem(pizza: Vector[String], nrow: Int, ncol: Int, minIngredients: Int, maxCells: Int, rowShit: Int = 0, colShift: Int = 0) {
  def ingredientAt(r: Int, c: Int): Char = pizza(r)(c)

  def splitInSmallerProblems: Iterable[Problem] = {
    for {
      startRow <- 0 until nrow by maxCells
      startCol <- 0 until ncol by maxCells
      zone = pizza.drop(startRow).take(maxCells).map(line => line.drop(startCol).take(maxCells))
    } yield Problem(zone, zone.size, zone.head.size, minIngredients, maxCells, startRow, startCol)
  }
}
