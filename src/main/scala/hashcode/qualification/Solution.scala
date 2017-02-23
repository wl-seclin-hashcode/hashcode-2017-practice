package hashcode.qualification

import java.io.PrintStream
import java.nio.file.{ Files, Paths }

import grizzled.slf4j.Logging
import rapture.json._
import rapture.json.jsonBackends.jawn._

case class Solution(serverAffectations: Vector[ServerAffectation]) {

  def writeToFile(name: String): Unit = {
    val ps = new PrintStream(name + ".json")
    ps.println(Json(this).toString)
    ps.close()
  }

}

case class CachedVideo(video: Video, cacheServer: Int, score: Int = 0) {
  val cost = video.size
}

case class ServerAffectation(cacheServer: Int, videos: Vector[Int])

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
