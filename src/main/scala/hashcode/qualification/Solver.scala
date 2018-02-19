package hashcode.qualification

import grizzled.slf4j.Logging
import scala.util.Random

object Solver extends Logging {
  def solveWithKnapsack(problem: Problem) = ???

  def gain(problem: Problem, cacheId: Int, videoId: Int): Int = {
    def gainForEndpoint(endpointId: Int) = problem.endpoints(endpointId).latencySavedPerCacheServer(cacheId)

    def gainForRequest(request: Request) = request.count * gainForEndpoint(request.endpointId)

    problem.requestsPerVideo(videoId).map(gainForRequest).sum
  }

  def solveCache(problem: Problem, cacheId: Int): Set[Int] = {
    val nbVideos = problem.videoSizes.size
    val allVideoIds = 0 until nbVideos
    val (_, bestVideoIds) = Knapsack.solve[Int](allVideoIds, gain(problem, cacheId, _), problem.videoSizes, problem.cacheCapacity)
    bestVideoIds
  }


  def solve(problem: Problem): Solution = {
    import problem._

    def videosSelect(videos: List[Video], remainingSize: Int, selected: Vector[Int] = Vector()): Vector[Int] = videos match {
      case Nil => selected
      case h :: t =>
        if (h.size <= remainingSize) videosSelect(t, remainingSize - h.size, h.id +: selected)
        else videosSelect(t, remainingSize, selected)
    }

    val affectations = for {
      cacheId <- 0 until caches
    } yield {
      val requests = requestsPerCacheServer(cacheId)
      val ids = requests.map(_.videoId).distinct // TODO : keep count
      val videos = Random.shuffle(ids.map(problem.videos))
      val selected = videosSelect(videos.toList, cacheCapacity)
      debug(s"for server $cacheId, using ${selected.size} videos out of ${ids.size}")
      ServerAffectation(cacheId, selected)
    }
    Solution(affectations.toVector)
  }


}