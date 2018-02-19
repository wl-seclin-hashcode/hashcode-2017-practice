package hashcode.qualification

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Scanner

import rapture._
import core._
import json._
import jsonBackends.jawn._
import formatters.humanReadable._
import rapture.data.Serializer

object Parser {
  implicit val ser = implicitly[Serializer[Endpoint, Json]]
//  implicit val ext = implicitly[Extractor[Endpoint, Json]]


  def read(f: String): Problem =
//    if (path(f).toFile.exists) {
//      val content = io.Source.fromFile(path(f).toFile).getLines.mkString
//      val p = Json.parse(content).as[Problem]
//      ???
//    }else
    {
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

      val serialized = Json(p).toString
      Files.write(path(f), serialized.getBytes(StandardCharsets.UTF_8))

      p

    }

  private def path(f: String) = {
    Paths.get(f + ".json")
  }

}