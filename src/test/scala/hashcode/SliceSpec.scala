package hashcode

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Shrink._
import scala.Vector
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink

@RunWith(classOf[JUnitRunner])
class SliceSpec extends FlatSpec with Matchers with PropertyChecks {
  val problem = Problem(Vector("TTTT", "MMMM"), 2, 4, 1, 2)

  "2 Slices" should "intersect properly" in {
    forAll { (slice1: Slice, slice2: Slice) =>
      slice1.intersects(slice2) shouldBe slice1.cells.exists(slice2.cells.contains)
    }
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

  def genSlice: Gen[Slice] = {
    for {
      a <- Gen.chooseNum(1, 200)
      b <- Gen.chooseNum(a + 1, 200)
      c <- Gen.chooseNum(1, 200)
      d <- Gen.chooseNum(c + 1, 200)
    } yield Slice(a, b, c, d)
  }

  //  def genLeek: Gen[Leek] =
  //    for {
  //      life <- Gen.chooseNum(1, 1000)
  //      damage <- Gen.chooseNum(1, 1000)
  //    } yield Leek(life, damage)
  //
  implicit val arbitrarySlice = Arbitrary(genSlice)

  implicit val shrinkSlice: Shrink[Slice] = Shrink {
    case Slice(a, b, c, d) => for {
      (a1, b1, c1, d1) <- shrink((a, b, c, d))
    } yield Slice(a1, b1, c1, d1)
  }

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100,
    maxDiscardedFactor = 15)

}