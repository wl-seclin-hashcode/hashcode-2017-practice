package hashcode.qualification

import grizzled.slf4j.Logging

object Solver extends Logging {

  def solve(problem: Problem): Solution = {
    import problem._

    def isUsed(video: Video, endpointsForCache: Vector[Endpoint]): Boolean = {
      //endpointsForCache.exists(ep => requestPerEndpointPerVideo(ep.id).contains(video.id))
      true
    }

    def videosSelect(endpointsForCache: Vector[Endpoint], videos: List[Video], remainingSize: Int, selected: Vector[Int] = Vector()): Vector[Int] = videos match {
      case Nil => selected
      case h :: t =>
        if (h.size <= remainingSize && isUsed(h, endpointsForCache)) videosSelect(endpointsForCache, t, remainingSize - h.size, h.id +: selected)
        else videosSelect(endpointsForCache, t, remainingSize, selected)
    }

    val affectations = for {
      cacheId <- 0 until caches
      endpointsForCache = endpoints.filter(_.cacheServers.contains(cacheId))
    } yield ServerAffectation(cacheId, videosSelect(endpointsForCache, videosSortedByCount.toList, cacheCapacity))
    Solution(affectations.toVector)
  }

}