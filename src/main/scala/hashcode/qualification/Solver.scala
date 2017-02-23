package hashcode.qualification

import grizzled.slf4j.Logging

object Solver extends Logging {

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
      requests = requestsPerCacheServer(cacheId)
      videos = requests.map(r => problem.videos(r.videoId)) //.sortBy(v => -videosCount(v.id))
    } yield ServerAffectation(cacheId, videosSelect(videos.toList, cacheCapacity))
    Solution(affectations.toVector)
  }

}