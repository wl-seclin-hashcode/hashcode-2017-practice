package hashcode.qualification

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import java.util.Scanner

import play.api.libs.json.Json._


object Parser {

  def read(f: String): Problem =
    if (path(f).toFile.exists) {
      val content = io.Source.fromFile(path(f).toFile).getLines.mkString
      parse(content).as[Problem]
    } else {
      val scan = new Scanner(new File(f))

      val v = scan.nextInt()
      val e = scan.nextInt()
      val r = scan.nextInt()
      val c = scan.nextInt()
      val x = scan.nextInt()

      scan.nextLine()

      val videoSizes = scan.nextLine().split(" ").map(_.toInt).toVector

      def readEndpoint(id: Int): Endpoint = {
        val l = scan.nextInt()
        val k = scan.nextInt()
        scan.nextLine()
        val latencies = (for {
          i <- 0 until k
          line = scan.nextLine()
          Array(cacheId, lat) = line.split(" ").map(_.toInt)
        } yield cacheId -> lat).toMap
        Endpoint(id, l, latencies)
      }

      val endpoints = Vector.tabulate(e)(readEndpoint)

      val requests = Vector.fill(r) {
        val Array(v, e, n) = scan.nextLine().split(" ").map(_.toInt)
        Request(v, e, n)
      }

      val p = Problem(c, x, videoSizes, endpoints, requests)

      val serialized = stringify(toJson(p))
      Files.write(path(f), serialized.getBytes(UTF_8))

      p

    }

  private def path(f: String) = {
    Paths.get(f + ".json")
  }

}