package hashcode

import grizzled.slf4j.Logging

object Solver extends Logging {
  def solve(problem: Problem): Solution = {
    import problem._
    val slices = for {
      c <- 0 until ncol
    } yield Slice(0, nrow min maxCells, c, c + 1)

    debug(slices)
    Solution(slices.filter(s => Validator.checkSlice(s, problem).isEmpty))
  }

}