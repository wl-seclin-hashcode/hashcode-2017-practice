package hashcode.qualification

import grizzled.slf4j.Logging
import play.api.libs.json.Json._
import play.api.libs.json._

import scala.util.Random

case class Problem(
                    caches: Int,
                    cacheCapacity: Int,
                    videoSizes: Vector[Int],
                    endpoints: Vector[Endpoint],
                    reqs: Vector[Request]) extends Logging {

  def gainForRequest(solution: Solution, requestId: Int): Int = {
    val request = reqs(requestId)
    val endpoint = endpoints(request.endpointId)
    request.count * (endpoint.latency - latency(request.endpointId, request.videoId, solution))
  }


  def latency(endpointId: Int, videoId: Int, solution: Solution): Int = {
    val endpoint = endpoints.find(_.id == endpointId).get
    val cacheServers = solution.serverAffectations.filter(sa ⇒ sa.videos.contains(videoId)).map(_.cacheServer)
    val latencies = endpoint.latency +: cacheServers.flatMap(cache ⇒ endpoint.serverLatencies.get(cache))
    latencies.min
  }

  val requests = {
    val uniques = reqs.groupBy(r => (r.endpointId, r.videoId)).map {
      case ((ep, vi), vect) => Request(vi, ep, vect.map(_.count).sum)
    }
    uniques.toVector
  }

  val videos = videoSizes.zipWithIndex.map { case (s, id) => Video(id, s) }

  val requestsPerEndpoint: Map[Int, Vector[Request]] = requests.groupBy(_.endpointId).withDefaultValue(Vector())

  val endpointsPerCacheServer: Map[Int, Vector[Endpoint]] = {
    val all = for {
      e <- endpoints.toVector
      c <- e.cacheServers
    } yield (e, c)

    val byCache = all.groupBy(_._2)

    byCache.mapValues(_.map(_._1))
  }

  val cacheServersPerEndpoint: Map[Int, Vector[Int]] = {
    val all = for {
      e <- endpoints.toVector
      c <- e.cacheServers
    } yield (c, e.id)

    val byEp = all.groupBy(_._2)

    byEp.mapValues(_.map(_._1))
  }.withDefaultValue(Vector())

  val requestsPerCacheServer: Map[Int, Vector[Request]] = {
    val all = for {
      r <- requests
      c <- cacheServersPerEndpoint(r.endpointId)
    } yield (r, c)

    val byCache = all.groupBy(_._2)

    byCache.mapValues(_.map(_._1))
  }

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


  def gain(cacheId: Int, videoId: Int): Int = {
    def gainForEndpoint(endpointId: Int) = endpoints(endpointId).latencySavedPerCacheServer(cacheId)

    def gainForRequest(request: Request) = request.count * gainForEndpoint(request.endpointId)

    requestsPerVideo(videoId).map(gainForRequest).sum
  }


  def solve: Solution = {

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
      val vids = Random.shuffle(ids.map(videos))
      val selected = videosSelect(vids.toList, cacheCapacity)
      debug(s"for server $cacheId, using ${selected.size} videos out of ${ids.size}")
      ServerAffectation(cacheId, selected)
    }
    Solution(affectations.toVector)
  }
}

object Problem {
  import Video._
  import Endpoint._
  import Request._
  implicit val problemReads: Reads[Problem] = reads[Problem]
  implicit val problemWrites: OWrites[Problem] = writes[Problem]
}

case class Video(id: Int, size: Int)


object Video {
  implicit val videoReads: Reads[Video] = reads[Video]
  implicit val videoWrites: OWrites[Video] = writes[Video]
}

case class Endpoint(id: Int, latency: Int, serverLatencies: Map[Int, Int]) {
  val latencySavedPerCacheServer: Map[Int, Int] =
    serverLatencies.mapValues(l => latency - l).withDefaultValue(0)

  val cacheServers = latencySavedPerCacheServer.keySet
}

object Endpoint {

  implicit val mapReads: Reads[Map[Int, Int]] = new Reads[Map[Int, Int]] {
    def reads(jv: JsValue): JsResult[Map[Int, Int]] =
      JsSuccess(jv.as[Map[String, Int]].map { case (k, v) =>
        Integer.parseInt(k) -> v.asInstanceOf[Int]
      })
  }

  implicit val mapWrites: Writes[Map[Int, Int]] = new Writes[Map[Int, Int]] {
    def writes(map: Map[Int, Int]): JsValue =
      Json.obj(map.map { case (s, o) =>
        val ret: (String, JsValueWrapper) = s.toString -> o
        ret
      }.toSeq: _*)
  }

  implicit val mapFormat: Format[Map[Int, Int]] = Format(mapReads, mapWrites)

  implicit val endpointReads: Reads[Endpoint] = reads[Endpoint]
  implicit val endpointWrites: OWrites[Endpoint] = writes[Endpoint]
}

case class Request(videoId: Int, endpointId: Int, count: Int)

object Request {
  implicit val requestReads: Reads[Request] = reads[Request]
  implicit val requestWrites: OWrites[Request] = writes[Request]
}