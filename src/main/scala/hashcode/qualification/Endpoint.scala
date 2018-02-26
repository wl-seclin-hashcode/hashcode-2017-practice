package hashcode.qualification

import play.api.libs.json.Json._
import play.api.libs.json._


case class Endpoint(id: Int, latency: Int, serverLatencies: Map[Int, Int]) {
  val latencySavedPerCacheServer: Map[Int, Int] =
    serverLatencies.mapValues(l => latency - l).withDefaultValue(0)

  val cacheServers = latencySavedPerCacheServer.keySet
}

object Endpoint {
  /**
    * Boilerplate to convert Map[Int,Int] to json maps which requires String keys.
    */
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

  implicit val fmt = format[Endpoint]
}