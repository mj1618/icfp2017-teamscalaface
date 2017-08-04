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
import java.nio.charset.StandardCharsets
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

  /* returns empty string at end of stream */
  def receive(in: BufferedInputStream) : String = {
    var rec = ""
    var n = ""
    println("receiving")
    breakable { 
    	for( a <- 1 to 10){
	    	val c = in.read
	    	if(c >= '0' && c <= '9') {
	    		n = n + c.asInstanceOf[Char]
	    	} else {
	    		break
	    	}
	    }
	  }
	  if (n == "") {
	    return ""
	  }
	  println("reading "+n+" chars")
	  val buffer = new Array[Byte]( n.toInt )
	  val x = in.read(buffer)
    val s = new String(buffer, StandardCharsets.UTF_8)
    println("received: "+ s)
    return s
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
  def init(out: PrintWriter, in: BufferedInputStream, offline: Boolean = false) : GameState = {

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
    val game = GameState(handleCirceResponse(decode[R_setup](receive(in))))

    if (offline) {
      val ready = buildPacket(T_setup(game.setup.punter).asJson.noSpaces);
      send(ready, out)
    } else {
      val ready = buildPacket(OT_setup(game.setup.punter, game).asJson.noSpaces);
      send(ready, out)
    }

    return game
  }

  // waits for R_gameplay from the server, sends it to a callback, and sends
  // the server back the callback's response. Callback must accept HCursor (a
  // cursor to a JSON list) and return JSON string
  def move(out: PrintWriter, in: BufferedInputStream, punter: PunterId, move_f: (PunterId, HCursor) => T_gameplay) = {
    println("move: waiting for prompt from server")
    val play: Json = handleCirceResponse(parse(receive(in)))
    val cursor: HCursor = play.hcursor

    if (cursor.fieldSet.getOrElse(null).contains("move")) {
      val play_list: HCursor = cursor.downField("move").downField("moves").success.getOrElse(null)
      val response = buildPacket(move_f(punter, play_list).asJson.noSpaces)
      send(response, out)
    } else if (cursor.fieldSet.getOrElse(null).contains("stop")) {
      println("move: server sent stop message: " + play)
      System.exit(0)
    } else {
      println("move: unknown server message: " + play)
    }
  }

}
