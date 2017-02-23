package hashcode.qualification

case class Problem(
    caches: Int,
    cacheCapacity: Int,
    videoSizes: Vector[Int],
    endpoints: Vector[Endpoint],
    requests: Vector[Request]) {

  val videos = videoSizes.zipWithIndex.map { case (s, id) => Video(id, s) }

  val requestsPerEndpoint: Map[Int, Vector[Request]] = requests.groupBy(_.endpointId).withDefaultValue(Vector())

  val requestsPerVideo: Map[Int, Vector[Request]] = requests.groupBy(_.videoId).withDefaultValue(Vector())

  val videosCount: Map[Int, Int] = requestsPerVideo.mapValues(reqs => reqs.map(_.count).sum).withDefaultValue(0)

  val videosSortedByCount = videos.sortBy(v => -videosCount(v.id))

  val requestPerEndpointPerVideo: Map[Int, Map[Int, Request]] = requestsPerEndpoint.mapValues {
    reqs =>
      val byVideo = reqs.groupBy(_.videoId)

      byVideo.mapValues { reqs =>
        val s = reqs.map(_.count).sum
        reqs.head.copy(count = s)
      }

  }
}

case class Video(id: Int, size: Int)

case class Endpoint(id: Int, latency: Int, serverLatencies: Map[Int, Int]) {
  val latencySavedPerCacheServer: Map[Int, Int] =
    serverLatencies.mapValues(l => latency - l)

  val cacheServers = latencySavedPerCacheServer.keySet
}

case class Request(videoId: Int, endpointId: Int, count: Int)
