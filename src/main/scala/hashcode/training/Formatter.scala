package hashcode.training

import java.io.PrintStream

import grizzled.slf4j.Logging

object Formatter extends Logging {
  def write(solution: Solution, score: Int, name: String): Unit = {
    val file = s"$name.$score.out"
    val f = new PrintStream(file)
    f.println(solution.slices.size)
    f.println(solution.slices.mkString("\n"))
    f.close
    info(s"wrote to $file")
  }
}