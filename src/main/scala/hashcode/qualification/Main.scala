package hashcode.qualification

import grizzled.slf4j.Logging

object Main extends App with Logging {
  val names = Seq(
    "qualification/example.in")
  info(names.map(solveIt).sum + s" total score for $names ")

  def solveIt(n: String): Int = {
    info(s"starting problem $n")
    val problem = Parser.read(n)
    val solution = Solver.solve(problem)

    Validator.score(solution, problem) match {
      case Left(score) =>
        info(s"score for $n : $score")
        Formatter.write(solution, score, n)
        score
      case Right(err) =>
        error(s"validation error for $n : $err")
        0
    }
  }

}