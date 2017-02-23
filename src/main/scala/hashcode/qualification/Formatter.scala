package hashcode.qualification

import java.io.PrintStream

import grizzled.slf4j.Logging

object Formatter extends Logging {

  def write(solution: Solution, score: Int, name: String): Unit = {
    val file = s"$name.$score.out".replace(".in", "")
    val f = new PrintStream(file)
    f.println(solution.serverAffectations.size)
    for {
      a <- solution.serverAffectations
    } {
      f.print(a.cacheServer + " ")
      f.println(a.videos.mkString(" "))
    }
    f.close()
    info(s"wrote to $file")
  }

}