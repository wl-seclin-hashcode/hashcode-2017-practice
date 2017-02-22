package hashcode

import grizzled.slf4j.Logging
import scala.annotation.tailrec
import scala.util.Random

object Solver extends Logging {
  def solve(problem: Problem): Solution = {
    val initial = Solution.readFromFile(problem.name) match {
      case Some(sol) => sol
      case None =>
        val subProblems = problem.splitInSmallerProblems
        info(s"split into ${subProblems.size} sub problems")
        if (subProblems.size == 1) solveSmall(problem)
        else {
          val smallSols = for {
            p <- subProblems.par
            slice <- solveSmall(p).slices
          } yield slice.shift(p.rowShit, p.colShift)
          Solution(smallSols.toList)
        }
    }
    info("initial solution : score = " + initial.usedCells.size)
    initial.writeToFile(problem.name)
    val filled = fill(initial, problem.validSlices)
    info("filled solution : score = " + filled.usedCells.size)
    //    improve(filled, problem, 0, 100)
    filled
  }

  def improve(s: Solution, p: Problem, i: Int, tries: Int): Solution = {
    if (i == s.slices.size)
      if (tries == 0) s
      else improve(s, p, 0, tries - 1)
    else {
      val (before, after) = s.slices.splitAt(i)
      val someSlices = before ++ after.drop(1)
      val another = fill(Solution(someSlices), p.validSlices.toList)
      val newScore = another.usedCells.size
      val oldScore = s.usedCells.size
      if (newScore > oldScore) {
        info(s"better score found $newScore")
        improve(another, p, 0, tries - 1)
      } else improve(s, p, i + 1, tries)
    }
  }

  def fill(s: Solution, candidates: List[Slice]): Solution = candidates match {
    case Nil => s
    case h :: t =>
      if (!h.cells.exists(s.usedCells.contains)) {
        fill(s + h, t)
      } else fill(s, t)
  }

  def solveSmall(problem: Problem): Solution = {
    import problem._
    val all = problem.validSlices

    @tailrec
    def filterRec(candidates: List[Slice], selected: Vector[Slice]): Vector[Slice] =
      candidates match {
        case Nil => selected
        case h :: t =>
          val rest = t.filterNot(_.intersects(h))
          filterRec(rest, selected :+ h)
      }
    print(".")
    Solution(filterRec(all, Vector()))
  }

}