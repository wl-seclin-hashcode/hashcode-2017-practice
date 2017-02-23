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
    } yield {
      val requests = requestsPerCacheServer(cacheId)
      val ids = requests.map(_.videoId).distinct // TODO : keep count
      val videos = ids.map(problem.videos).sortBy(v => -videosCount(v.id))
      val selected = videosSelect(videos.toList, cacheCapacity)
      debug(s"for server $cacheId, using ${selected.size} videos out of ${ids.size}")
      ServerAffectation(cacheId, selected)
    }
    Solution(affectations.toVector)
  }

}