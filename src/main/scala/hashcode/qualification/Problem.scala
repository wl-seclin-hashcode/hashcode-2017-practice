package hashcode.qualification

case class Problem(
    caches: Int,
    cacheCapacity: Int,
    videoSizes: Vector[Int],
    endpoints: Vector[Endpoint],
    requests: Vector[Request]) {

  val videos = videoSizes.zipWithIndex.map { case (s, id) => Video(id, s) }

  val requestsPerEndpoint: Map[Int, Vector[Request]] = requests.groupBy(_.endpointId)

  val requestPerEndpointPerVideo: Map[Int, Map[Int, Vector[Request]]] = requestsPerEndpoint.map {
    case (endpointId, reqs) => endpointId -> reqs.groupBy(_.videoId)
  }
}

case class Video(id: Int, size: Int)

case class Endpoint(latency: Int, serverLatencies: Map[Int, Int]) {
  val latencySavedPerCacheServer: Map[Int, Int] =
    serverLatencies.mapValues(l => latency - l)
}

case class Request(videoId: Int, endpointId: Int, count: Int)
