package hashcode.qualification

import play.api.libs.json.Json._

case class Video(id: Int, size: Int)

object Video {
  implicit val fmt = format[Video]
}