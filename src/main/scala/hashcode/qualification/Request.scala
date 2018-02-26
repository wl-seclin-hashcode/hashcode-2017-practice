package hashcode.qualification

import play.api.libs.json.Json._

case class Request(videoId: Int, endpointId: Int, count: Int)

object Request {
  implicit val fmt = format[Request]
}