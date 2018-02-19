package hashcode.qualification

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class SolverSpec extends FlatSpec with Matchers {

  val problem = Parser.read("qualification/example.in")

  "Solver" should "solve trivial problem" in {
    //    val sol = Solver.solve(problem)
    //
    //    val Left(score) = Validator.score(sol, problem)
    //    score shouldBe 0
  }

  "Solver" should "find gain" in {
    Solver.gain(problem, 0, 0) shouldBe 0
    Solver.gain(problem, 0, 1) shouldBe 900000
  }

  "Solver" should "optimize one cache - SC0" in {
    Solver.solveCache(problem, 0) shouldBe Set(1,3)
  }

  "Solver" should "solve problem by optimizing cache by cache" in {
    val solution = Solver.solveWithKnapsack(problem)
    Validator.score(solution, problem) shouldBe Left(562500)
  }

}