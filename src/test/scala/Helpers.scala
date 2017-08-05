import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scala.math.pow

import org.scalatest.{ FlatSpec, Matchers }
import lambda.traceur.onlinemsg.Msg
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.lamclient._
import lambda.traceur.helpers._
import lambda.traceur._
import java.io.PrintWriter
import sys.process._
import java.net.{ServerSocket, Socket}
import java.io._
import resource._
import scala.util.control.Breaks._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class HelpersSpec extends FlatSpec with Matchers {
  it should "convert game" in {
    var sample = scala.io.Source.fromFile("samples/sample1.json").mkString
    var setupString = """{"punter":1,"punters":2,"map":"""+sample+"}"
    var claim = """{"claim" : {"punter" : 2, "source" : 0, "target" : 1}}"""
    var stream = 
        """17:{"you":"blinken"}""" +
        setupString.length + ":" + setupString +
        claim.length + ":" + claim
    val write = new PrintWriter(new BufferedWriter(new StringWriter()))
    val read = new BufferedInputStream(new ByteArrayInputStream(stream.getBytes))
    val shake = LamClient.handshake(write, read)
    val brains = new MagicBrain()
    val (setup, game) = LamClient.init(write, read, brains)
    println("Recieved game: " + game)
    println(Helpers.mapToGraph(setup.map))
  }
}


