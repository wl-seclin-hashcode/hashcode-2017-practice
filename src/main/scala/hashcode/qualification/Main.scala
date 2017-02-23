package hashcode.qualification

import grizzled.slf4j.Logging

object Main extends App with Logging {
  val names = Seq(
    "qualification/kittens.in",
    "qualification/me_at_the_zoo.in",
    "qualification/trending_today.in",
    "qualification/videos_worth_spreading.in")
  info(names.map(solveIt).sum + s" total score for $names ")

  def solveIt(n: String): Int = {
    info(s"starting problem $n")
    val problem = Parser.read(n)
    println(problem)

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