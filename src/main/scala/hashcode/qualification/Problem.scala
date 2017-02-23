package hashcode.qualification

case class Problem(
    caches: Int,
    cacheCapacity: Int,
    videoSizes: Vector[Int],
    endpoints: Vector[Endpoint],
    requests: Vector[Request]) {

  val videos = videoSizes.zipWithIndex.map { case (id, s) => Video(id, s) }

}

case class Video(id: Int, size: Int)

case class Endpoint(latency: Int, serverLatencies: Map[Int, Int])

case class Request(videoId: Int, endpointId: Int, count: Int)
