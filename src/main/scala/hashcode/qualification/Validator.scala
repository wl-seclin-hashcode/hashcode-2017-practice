package hashcode.qualification

object Validator {

  def score(solution: Solution, problem: Problem): Either[Int, String] = {
    val requestsCount = problem.reqs.size
    val allRequestsId = problem.reqs.indices
    val totalGain = allRequestsId.map(requestId ⇒ problem.gainForRequest(solution, requestId)).sum
    val score = totalGain / requestsCount
    Left(score)
  }

}