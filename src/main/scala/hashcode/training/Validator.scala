package hashcode.training

object Validator {
  def score(solution: Solution, problem: Problem): Either[Int, String] = {

    val validation = solution.slices.foldLeft(OngoingValidation()) {
      case (ongoing @ OngoingValidation(score, used, failure), slice) =>
        if (failure.isDefined) ongoing
        else {
          val cells = slice.cells
          checkSlice(slice, problem) match {
            case None =>
              if (cells.exists(used.contains)) OngoingValidation(0, Set(), Some(slice + " uses cells which are already in use"))
              else OngoingValidation(score + cells.size, used ++ cells)
            case err => OngoingValidation(0, Set(), err)
          }
        }
    }
    validation.failure match {
      case Some(err) => Right(err)
      case None      => Left(validation.score)
    }
  }

  def checkSlice(slice: Slice, problem: Problem): Option[String] = {
    val cells = slice.cells
    cells.collect { case p @ Point(r, c) if r >= problem.nrow || c >= problem.ncol => p }.headOption match {
      case Some(Point(r, c)) => Some(s"invalid slice : $r,$c is out of bounds")
      case _ =>
        val mushroom = cells.count { case Point(r, c) => problem.ingredientAt(r, c) == 'M' }
        val tomato = cells.count { case Point(r, c) => problem.ingredientAt(r, c) == 'T' }
        if (mushroom < problem.minIngredients || tomato < problem.minIngredients) Some(slice + " does not use enough ingredients")
        else if (cells.size > problem.maxCells) Some(slice + " is too big")
        else None
    }
  }
}

case class OngoingValidation(score: Int = 0, usedCells: Set[Point] = Set(), failure: Option[String] = None)

