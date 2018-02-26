
import hashcode.qualification._
import play.api.libs.json.Json._

case class Person(name: String, age: Int)

implicit val fmt = format[Person]

stringify(toJson(List(1, 2, 3, 4)))
stringify(toJson(Map(1 -> 2, 3 -> 4)))
val pj = stringify(toJson(Person("aa", 2)))

parse(pj).as[Person]



stringify(toJson(Video(1, 500)))


val ep = stringify(toJson(Endpoint(1, 500, Map(1 -> 2, 3 -> 4))))

parse(ep).as[Endpoint]

val problem = Problem(caches = 3,
  cacheCapacity = 100,
  videoSizes = Vector(5),
  endpoints = Vector(Endpoint(1, 500, Map(1 -> 2, 3 -> 4))),
  reqs = Vector(Request(videoId = 1, endpointId = 2, count = 100)))


val pr=stringify(toJson(problem))

parse(pr).as[Problem]