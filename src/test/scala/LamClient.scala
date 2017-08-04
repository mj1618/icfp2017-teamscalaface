import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scala.math.pow

import org.scalatest.{ FlatSpec, Matchers }
import lambda.traceur.onlinemsg.Msg
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.lamclient._
import java.io.PrintWriter
import sys.process._
import java.net.{ServerSocket, Socket}
import java.io._
import resource._
import scala.util.control.Breaks._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
class LamClientSpec extends FlatSpec with Matchers {
  def sampleCallback(punter: PunterId, play: HCursor) : T_gameplay = {
    println("sampleCallback: punter " + punter + " got play: " + play.value);
    println("sampleCallback: sending move: " + T_gameplay(TR_claim_p(punter, 0, 1)).asJson.noSpaces)
    return T_gameplay(TR_claim_p(punter, 0, 1))
  }
  
  it should "parse messagess" in {
    var sample = scala.io.Source.fromFile("samples/sample1.json").mkString
    var setup = """{"punter":1,"punters":2,"map":"""+sample+"}"
    var claim = """{"claim" : {"punter" : 2, "source" : 0, "target" : 1}}"""
    var stream = 
        """17:{"you":"blinken"}""" +
        setup.length + ":" + setup +
        claim.length + ":" + claim
    val write = new PrintWriter(new BufferedWriter(new StringWriter()))
    // inStream <- managed(new InputStreamReader(connection.getInputStream))
    val read = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(stream.getBytes)))
    
    val game = LamClient.init(write, read)
    println("Recieved game: " + game)

    LamClient.move(write, read, game.setup.punter, sampleCallback)
  }
}


