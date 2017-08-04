import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.WUnDiEdge
import scalax.collection.GraphPredef._

import java.io.PrintWriter
import sys.process._
import java.net.{ServerSocket, Socket}
import java.io._
import resource._
import scala.util.control.Breaks._

import lambda.traceur.onlinemsg.Msg
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.lamclient._
import lambda.traceur.helpers.Helpers._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Application {
  
  // this thing needs to do the game logic -blinken
  // right now it always attempts to claim (0,1)
  def sampleCallback(punter: PunterId, play: HCursor) : T_gameplay = {
    debug("sampleCallback: punter " + punter + " got play: " + play.value.noSpaces);
    debug("sampleCallback: sending move: " + T_gameplay(TR_claim_p(punter, 0, 1)).asJson.noSpaces)
    return T_gameplay(TR_claim_p(punter, 0, 1))
  }

  def main(args : Array[String]) : Unit = {
    val server : String = if (args.length == 2) args(0) else "punter.inf.ed.ac.uk"
    val port : Int = if (args.length == 2) args(1).toInt else 9001
    println("main: connecting to " + server + " on port " + port)

    for { connection <- managed(new Socket(server, port))
      outStream <- managed(connection.getOutputStream)
      val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)))
      in <- managed(new BufferedInputStream(connection.getInputStream))
    } {
      val game = LamClient.init(out, in, false)
      debug("main: recieved game: " + game)

      // send moves forever
      while (true) { LamClient.move(out, in, game.setup.punter, sampleCallback) }
    }
  }
}

object LocalApplication {
  def main(args: Array[String]) : Unit = {
    val out = new PrintWriter(new BufferedWriter(new StringWriter()))
    //var sample = scala.io.Source.fromFile("samples/sample1.json").mkString
	//	var setup = """{"punter":1,"punters":2,"map":"""+sample+"}"
    //var stream = 
    // 	"""17:{"you":"blinken"}""" +
    //	setup.length + ":" + setup +
    //	"""{"claim" : {"punter" : 2, "source" : 0, "target" : 1}}"""
    //stream <- managed(new InputStreamReader(connection.getInputStream))
    //val in = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(stream.getBytes)))
    
  }
}
