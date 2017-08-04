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
import lambda.traceur.gameserver._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Application {
  
  // this thing needs to do the game logic -blinken
  // right now it always attempts to claim (0,1)
  def sampleCallback(punter: PunterId, play: HCursor) : T_gameplay = {
    println("sampleCallback: punter " + punter + " got play: " + play.value);
    println("sampleCallback: sending move: " + T_gameplay(TR_claim_p(punter, 0, 1)).asJson.noSpaces)
    return T_gameplay(TR_claim_p(punter, 0, 1))
  }

  def main(args : Array[String]) : Unit = {
    for { connection <- managed(new Socket("punter.inf.ed.ac.uk", 9001))
      outStream <- managed(connection.getOutputStream)
      val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)))
      inStream <- managed(new InputStreamReader(connection.getInputStream))
      val in = new BufferedReader(inStream)
    } {
      val game = LamClient.init(out, in)
      println("Recieved game: " + game)

      // send moves forever
      while (true) { LamClient.move(out, in, game.punter, sampleCallback) }
    }
  }
}

object OfflineApplication {
  def main(args: Array[String]) : Unit = {
    
  }
}
