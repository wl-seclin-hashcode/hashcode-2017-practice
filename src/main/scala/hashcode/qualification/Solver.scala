package hashcode.qualification

import grizzled.slf4j.Logging

object Solver extends Logging {

  def solve(problem: Problem): Solution = {
    import problem._

    val all = allCachedVideos(problem)
    println(s"${all.size} possible video caches")
    println(all.take(10).mkString("\n"))
    val cachedVideos = selectBestCachedVideos(all)

    val affectations = for {
      (cacheId, videos) <- cachedVideos.groupBy(_.cacheServer)
    } yield ServerAffectation(cacheId, videos.map(_.video.id))
    Solution(affectations.toVector)
  }

  def allCachedVideos(p: Problem): Vector[CachedVideo] = {
    def score(cv: CachedVideo) = {
      for {
        (e, eid) <- p.endpoints.zipWithIndex
        saved <- e.latencySavedPerCacheServer.get(cv.cacheServer)
        request <- p.requestPerEndpointPerVideo(eid).get(cv.video.id)
      } yield saved * request.count
    }.sum
    for {
      v <- p.videos
      c <- 0 until p.caches
      cv = CachedVideo(v, c)
    } yield CachedVideo(v, c, score(cv))
  }

  def selectBestCachedVideos(all: Vector[CachedVideo]): Vector[CachedVideo] = {
    all.take(1)
  }

  def videosSelect(videos: List[Video], remainingSize: Int, selected: Vector[Int] = Vector()): Vector[Int] = videos match {
    case Nil => selected
    case h :: t =>
      if (h.size <= remainingSize) videosSelect(t, remainingSize - h.size, h.id +: selected)
      else videosSelect(t, remainingSize, selected)
  }

}

