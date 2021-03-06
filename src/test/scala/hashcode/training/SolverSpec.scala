package hashcode.training

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[JUnitRunner])
class SolverSpec extends FlatSpec with Matchers with PropertyChecks {
  val problem = Problem(Vector("TTTT", "MMMM"), 2, 4, 1, 2)

  "Solver" should "solve trivial problem" in {
    val sol = Solver.solve(problem)

    val Left(score) = Validator.score(sol, problem)
    score shouldBe 8
  }

  it should "solve the example" in {
    val example = Parser.read("training/example.in")
    val sol = Solver.solve(example)

    val Left(score) = Validator.score(sol, example)
    score shouldBe 15
  }

  "Problem" should "split correctly" in {
    Problem(Vector("TT", "MM"), 2, 2, 1, 2).splitInSmallerProblems.size shouldBe 1
    problem.splitInSmallerProblems.size shouldBe 2
  }

  //  it should "return >0 for team with a stronger leek (damage)" in {
  //    forAll(genTeam, Gen.chooseNum(1, 1000)) { (team1: Seq[Leek], i: Int) =>
  //      whenever(team1.nonEmpty) {
  //        val l = team1.head
  //        val betterLeek = l.copy(damage = l.damage + i)
  //        val betterTeam = team1.tail :+ betterLeek
  //        evaluation.compare(betterTeam, team1) should be > 0.0
  //      }
  //    }
  //  }

  //  def genTeam: Gen[Seq[Leek]] = {
  //    for {
  //      n <- Gen.chooseNum(1, 8)
  //      l <- Gen.listOfN(n, genLeek)
  //    } yield l
  //  }
  //
  //  def genLeek: Gen[Leek] =
  //    for {
  //      life <- Gen.chooseNum(1, 1000)
  //      damage <- Gen.chooseNum(1, 1000)
  //    } yield Leek(life, damage)
  //
  //  implicit val arbitraryLeek = Arbitrary(genLeek)
  //
  //  implicit def shrinkLeek: Shrink[Leek] = Shrink {
  //    case Leek(a, b) => for {
  //      (a1, b1) <- shrink((a, b))
  //    } yield Leek(a1, b1)
  //  }

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100,
    maxDiscardedFactor = 15)

}