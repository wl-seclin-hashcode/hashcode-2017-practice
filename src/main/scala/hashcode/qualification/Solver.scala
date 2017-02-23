package hashcode.qualification

import grizzled.slf4j.Logging

object Solver extends Logging {

  def solve(problem: Problem): Solution = {
    import problem._

    def isUsed(video: Video, cacheId: Int): Boolean = {
      val endpointsForCache = problem.endpoints.filter(_.cacheServers.contains(cacheId))
      endpointsForCache.exists(ep => problem.requestPerEndpointPerVideo(ep.id).contains(video.id))
    }

    def videosSelect(cacheId: Int, videos: List[Video], remainingSize: Int, selected: Vector[Int] = Vector()): Vector[Int] = videos match {
      case Nil => selected
      case h :: t =>
        if (h.size <= remainingSize && isUsed(h, cacheId)) videosSelect(cacheId, t, remainingSize - h.size, h.id +: selected)
        else videosSelect(cacheId, t, remainingSize, selected)
    }

    val affectations = for {
      cacheId <- 0 until problem.caches
    } yield ServerAffectation(cacheId, videosSelect(cacheId, videos.sortBy(_.size).toList, cacheCapacity))
    Solution(affectations.toVector)
  }

}