package hashcode.qualification

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class SolverSpec extends FlatSpec with Matchers {
  val problem = Problem()

  "Solver" should "solve trivial problem" in {
    val sol = Solver.solve(problem)

    val Left(score) = Validator.score(sol, problem)
    score shouldBe 0
  }

}