package hashcode.qualification

import java.io.PrintStream

import grizzled.slf4j.Logging

object Formatter extends Logging {

  def write(solution: Solution, score: Int, name: String): Unit = {
    val file = s"$name.$score.out".replace(".in","")
    val f = new PrintStream(file)
    f.close()
    info(s"wrote to $file")
  }

}