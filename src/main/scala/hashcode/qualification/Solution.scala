package hashcode.qualification

import java.io.PrintStream
import java.nio.file.{Files, Paths}

import grizzled.slf4j.Logging
import rapture.json._
import rapture.json.jsonBackends.jawn._

case class Solution() {

  def writeToFile(name: String): Unit = {
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

case class Point(r: Int, c: Int)