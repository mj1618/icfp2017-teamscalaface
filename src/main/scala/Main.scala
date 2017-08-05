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
import lambda.traceur._
import lambda.traceur.lamclient._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Application {
  
  def runOnline(server: String, port: Int) = {
    println("main: connecting to " + server + " on port " + port)

    try {
      for { connection <- managed(new Socket(server, port))
        outStream <- managed(connection.getOutputStream)
        val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)))
        in <- managed(new BufferedInputStream(connection.getInputStream))
      } {
        LamClient.runGame(out, in, new MagicBrain())
      }
    } catch {
      case e: java.net.ConnectException => { println("Error connecting: " + e) }
    }
  }

  def runOffline() = {
    val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(System.out)))
    val in = new BufferedInputStream(System.in)
    LamClient.runGameOffline(out, in, new MagicBrain())
  }

  def main(args : Array[String]) : Unit = {
    // sbt "run-main Application punter.inf.ed.ac.uk 9002"
    // sbt "run-main Application" # use defaults
    // sbt "run-main Application offline" # internet is for wimps
    if ((args.length == 1) && (args(0) == "offline")) {
      runOffline()
    } else {
      val server : String = "punter.inf.ed.ac.uk"
      val port : Int = if (args.length >= 1) args(0).toInt else 9101
      runOnline(server, port)
    }
  }
}

