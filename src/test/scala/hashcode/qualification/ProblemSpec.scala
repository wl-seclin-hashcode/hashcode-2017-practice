package hashcode.qualification

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class ProblemSpec extends FlatSpec with Matchers {
  val problem = Parser.read("qualification/example.in")

  val sol = Solution(Vector(
    ServerAffectation(0, Vector(2)),
    ServerAffectation(1, Vector(3, 1)),
    ServerAffectation(2, Vector(0, 1))
  ))

  //  "Validator" should "score trivial problem/solution" in {
  //    val sol = Solution(Vector(
  //      ServerAffectation(0, Vector(2)),
  //      ServerAffectation(1, Vector(3, 1)),
  //      ServerAffectation(2, Vector(0, 1))
  //    ))
  //    Validator.score(sol, problem) shouldBe Left(462500)
  //  }

  "Problem" should "compute latency in trivial problem/solution" in {
    problem.latency(solution = sol, endpointId = 0, videoId = 0) shouldBe 200
    problem.latency(solution = sol, endpointId = 0, videoId = 1) shouldBe 200
    problem.latency(solution = sol, endpointId = 0, videoId = 2) shouldBe 100
    problem.latency(solution = sol, endpointId = 0, videoId = 3) shouldBe 300
    problem.latency(solution = sol, endpointId = 0, videoId = 4) shouldBe 1000
  }
}