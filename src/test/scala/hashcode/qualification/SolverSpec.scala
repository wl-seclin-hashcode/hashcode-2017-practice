package hashcode.qualification

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class SolverSpec extends FlatSpec with Matchers {
  val sol = Solution(Vector(
    ServerAffectation(0, Vector(2)),
    ServerAffectation(1, Vector(3, 1)),
    ServerAffectation(2, Vector(0, 1))
  ))

  "Solver" should "solve trivial problem" in {
    //    val sol = Solver.solve(problem)
    //
    //    val Left(score) = Validator.score(sol, problem)
    //    score shouldBe 0
  }

  "Solver" should "find gain" in {
    val problem = Parser.read("qualification/example.in")
    Solver.gain(problem, 0, 0) shouldBe 0
    Solver.gain(problem, 0, 1) shouldBe 900000
  }

  "Solver" should "find gain" in {
    val problem = Parser.read("qualification/example.in")
    Solver.gain(problem, 0, 0) shouldBe 0
    Solver.gain(problem, 0, 1) shouldBe 900000
  }


}