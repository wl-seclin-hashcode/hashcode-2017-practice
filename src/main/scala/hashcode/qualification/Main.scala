package hashcode.qualification

import grizzled.slf4j.Logging

object Main extends App with Logging {
  val names = Seq(
    "qualification/me_at_the_zoo.in",
    "qualification/trending_today.in",
    "qualification/videos_worth_spreading.in",
    "qualification/kittens.in")
  info(names.map(solveIt).sum + s" total score for $names ")

  def solveIt(n: String): Int = {
    info(s"starting problem $n")
    val problem = Parser.read(n)
    info(s"parsed problem $n : ${problem.caches} cache servers ${problem.endpoints.size} endpoints ${problem.requests.size} requests")
    val solution = problem.solveWithKnapsack

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