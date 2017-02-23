package hashcode.qualification

case class Problem(videos: Int,
                   caches: Int,
                   cacheCapacity: Int,
                   videoSizes: Vector[Int],
                   endpoints: Vector[Endpoint],
                   requests: Vector[Request]) {

}

case class Endpoint(latency: Int, serverLatencies: Map[Int, Int])

case class Request(videoId: Int, endpointId: Int, count: Int)
