package hashcode

import grizzled.slf4j.Logging
import scala.annotation.tailrec

object Solver extends Logging {
  def solve(problem: Problem): Solution = {
    import problem._
    val all = validSlices(problem).toVector.sortBy(-_.size)
    debug(s"${all.size} possible slices")
    @tailrec
    def filterRec(candidates: Vector[Slice], selected: Vector[Slice]): Vector[Slice] =
      if (candidates.isEmpty) selected
      else {
        val s = candidates.head
        val rest = candidates.tail.filterNot(_.intersects(s))
        debug(rest.size + " slices to consider")
        filterRec(rest, selected :+ s)
      }
    Solution(filterRec(all, Vector()))
  }

  def validSlices(p: Problem): Iterable[Slice] = {
    import p._
    for {
      startRow <- 0 until nrow
      startCol <- 0 until ncol
      slice <- slicesStartingAt(startRow, startCol, p)
      if Validator.checkSlice(slice, p).isEmpty
    } yield slice
  }

  def slicesStartingAt(startRow: Int, startCol: Int, p: Problem): Iterable[Slice] = {
    import p._
    val slices = for {
      endRow <- startRow + 1 to (nrow min (startRow + maxCells))
      rows = endRow - startRow
      endCol <- startCol + 1 to (ncol min (startCol + maxCells / rows))
      cols = endCol - startCol
      if rows * cols <= p.maxCells
      slice = Slice(startRow, endRow, startCol, endCol)
    } yield slice
    slices
  }

}