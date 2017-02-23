package hashcode.qualification

import grizzled.slf4j.Logging

object Solver extends Logging {

  def solve(problem: Problem): Solution = {
    import problem._
    val affectations = for {
      cacheId <- 0 until problem.caches
    } yield ServerAffectation(cacheId, Vector())
    Solution(affectations.toVector)
  }

}