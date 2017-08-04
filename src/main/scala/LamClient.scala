package lambda
package traceur
package lamclient

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
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object LamClient {
  // import resource.ManagedResource
  def send(str: String, out: PrintWriter) : Unit = {
    println("sending: "+str)
    out.print(str)
    out.flush()
  }

  def receive(in: BufferedReader) : String = {
    var rec = ""
    var n = ""
    println("receiving")
    breakable { 
    	for( a <- 1 to 1000){
	    	val c = in.read.asInstanceOf[Char]
	    	if(c==':'){
	    		break
	    	} else {
	    		n = n + c
	    	}
	    }
	  }
	  println("reading "+n+" chars")
	  val buffer = new Array[ Char ]( n.toInt )
	  val x = in.read(buffer)
    println("received: "+buffer.mkString)
    return ""+buffer.mkString
  }

  def buildPacket(json: String) : String = {
    return json.length + ":" + json
  }

  def handleCirceResponse[T >: Null](response: Either[io.circe.Error, T]) : T = {
    response match {
      case Left(msg) => {
        println("Error decoding JSON: " + response)
        return null
      }
      case Right(msg) => return msg
    }
  }

  // handshake and get game state
  def init(out: PrintWriter, in: BufferedReader) : R_setup = {

    val name = "blinken"
    val hello = buildPacket(T_handshake(name).asJson.noSpaces);
    send(hello, out)

    val response = handleCirceResponse(decode[R_handshake](receive(in)))

    if (response.you != name) {
      println("Error connecting: got '" + response + "' from server")
      return null
    }

    println("Connected. Waiting for other player to join 🍆")

    // waiting for this may take some time
    val game = handleCirceResponse(decode[R_setup](receive(in)))

    val ready = buildPacket(T_setup(game.punter).asJson.noSpaces);
    send(ready, out)

    return game
  }

  // waits for R_gameplay from the server, sends it to a callback, and sends
  // the server back the callback's response. Callback must accept R_gameplay
  // and return JSON string
  def move(out: PrintWriter, in: BufferedReader, punter: PunterId, move_f: (PunterId, R_gameplay) => T_gameplay) = {
    println("move: waiting for prompt from server")
    val play = handleCirceResponse(decode[R_gameplay](receive(in)))
    val response = buildPacket(move_f(punter, play).asJson.noSpaces);
    send(response, out)
  }

}
