package hashcode

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Shrink._
import scala.Vector

@RunWith(classOf[JUnitRunner])
class ValidatorSpec extends FlatSpec with Matchers with PropertyChecks {
  val problem = Problem(Vector("TTTT", "MMMM"), 2, 4, 1, 2)

  "Validator" should "catch invalid slice with not enough mushroom" in {
    val sol = Solution(Seq(Slice(0, 0, 0, 1)))
    val Right(err) = Validator.score(sol, problem)

    err should endWith("does not use enough ingredients")
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