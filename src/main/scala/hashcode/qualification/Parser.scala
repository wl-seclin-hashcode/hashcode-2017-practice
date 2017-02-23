package hashcode.qualification

import java.io.File
import java.util.Scanner

object Parser {

  def read(f: String): Problem = {

    val scan = new Scanner(new File(f))

    val v = scan.nextInt()
    val e = scan.nextInt()
    val r = scan.nextInt()
    val c = scan.nextInt()
    val x = scan.nextInt()

    scan.nextLine()

    val videoSizes = scan.nextLine().split(" ").map(_.toInt).toVector

    def readEndpoint(): Endpoint = {
      val l = scan.nextInt()
      val k = scan.nextInt()
      scan.nextLine()
      val latencies = (for {
        i <- 0 until k
        line = scan.nextLine()
        Array(cacheId, lat) = line.split(" ").map(_.toInt)
      } yield cacheId -> lat).toMap
      Endpoint(l, latencies)
    }

    val endpoints = Vector.fill(e)(readEndpoint())

    val requests = Vector.fill(r) {
      val Array(v, e, n) = scan.nextLine().split(" ").map(_.toInt)
      Request(v, e, n)
    }

    Problem(c, x, videoSizes, endpoints, requests)

  }

}