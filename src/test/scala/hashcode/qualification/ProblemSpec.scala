package hashcode.qualification

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class ProblemSpec extends FlatSpec with Matchers {
  val problem = Parser.parse("qualification/example.in")

  val sol = Solution(Vector(
    ServerAffectation(0, Vector(2)),
    ServerAffectation(1, Vector(3, 1)),
    ServerAffectation(2, Vector(0, 1))
  ))

  "Validator" should "score trivial problem/solution" in {
    val sol = Solution(Vector(
      ServerAffectation(0, Vector(2)),
      ServerAffectation(1, Vector(3, 1)),
      ServerAffectation(2, Vector(0, 1))
    ))
    Validator.score(sol, problem) shouldBe Left(462500)
    // TODO : check that wrong solutions return Right
  }

  it should "compute latency in trivial problem/solution" in {
    problem.latency(solution = sol, endpointId = 0, videoId = 0) shouldBe 200
    problem.latency(solution = sol, endpointId = 0, videoId = 1) shouldBe 200
    problem.latency(solution = sol, endpointId = 0, videoId = 2) shouldBe 100
    problem.latency(solution = sol, endpointId = 0, videoId = 3) shouldBe 300
    problem.latency(solution = sol, endpointId = 0, videoId = 4) shouldBe 1000
  }

  it should "compute gain for request in trivial problem/solution" in {
    problem.gainForRequest(solution = sol, requestId = 1) shouldBe 0
    problem.gainForRequest(solution = sol, requestId = 2) shouldBe 0
    problem.gainForRequest(solution = sol, requestId = 0) shouldBe 700 * 1500
    problem.gainForRequest(solution = sol, requestId = 3) shouldBe 800 * 1000
  }

  it should "find gain" in {
    problem.gain(0, 0) shouldBe 0
    problem.gain(0, 1) shouldBe 900000
  }
}