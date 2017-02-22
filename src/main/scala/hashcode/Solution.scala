package hashcode

import rapture._
import core._, json._
import jsonBackends.jawn._
import formatters.humanReadable._
import java.io.PrintStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import grizzled.slf4j.Logging

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

case class Slice(row1: Int, row2: Int, col1: Int, col2: Int) {
  lazy val cells = (for {
    r <- row1 until row2
    c <- col1 until col2
  } yield Point(r, c)).toSet

  lazy val size = cells.size

  def intersects(that: Slice) = {
    that.cells.exists(cells.contains)
  }

  def shift(r: Int, c: Int) = Slice(row1 + r, row2 + r, col1 + c, col2 + c)

  override def toString = s"$row1 $col1 ${row2 - 1} ${col2 - 1}"
}

case class Point(r: Int, c: Int)