package hashcode.training

import java.io.PrintStream
import java.nio.file.{Files, Paths}

import grizzled.slf4j.Logging
import rapture.json._
import rapture.json.jsonBackends.jawn._

case class Solution(slices: Seq[Slice]) { self =>
  lazy val usedCells = slices.flatMap(_.cells).toSet

  def +(s: Slice) = new Solution(s +: slices) {
    override lazy val usedCells = self.usedCells ++ s.cells
  }

  def writeToFile(name: String) = {
    val ps = new PrintStream(name + ".json")
    ps.println(Json(this).toString)
    ps.close()
  }
}

object Solution extends Logging {
  def readFromFile(name: String): Option[Solution] = {
    val fileName = name + ".json"
    if (Files.exists(Paths.get(fileName))) Some {
      val content = io.Source.fromFile(fileName).getLines.mkString
      info(s"loaded from $fileName")
      Json.parse(content).as[Solution]
    }
    else None
  }
}

case class Slice(r1: Int, r2: Int, c1: Int, c2: Int) {
  val row1 = r1 min r2
  val row2 = r1 max r2
  val col1 = c1 min c2
  val col2 = c1 max c2

  lazy val cells = (for {
    r <- row1 until row2
    c <- col1 until col2
  } yield Point(r, c)).toSet

  lazy val size = cells.size

  def intersects(that: Slice) = {
    @inline def between(a: Int, b: Int, x: Int) = x >= a && x <= b
    @inline def intersects1d(x1: Int, x2: Int, a1: Int, a2: Int) =
      between(x1, x2, a1) || between(x1, x2, a2) ||
        between(a1, a2, x1) || between(a1, a2, x2)

    intersects1d(col1, col2 - 1, that.col1, that.col2 - 1) && intersects1d(row1, row2 - 1, that.row1, that.row2 - 1)
  }

  def shift(r: Int, c: Int) = Slice(row1 + r, row2 + r, col1 + c, col2 + c)

  override def toString = s"$row1 $col1 ${row2 - 1} ${col2 - 1}"
}

case class Point(r: Int, c: Int)