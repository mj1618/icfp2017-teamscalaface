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
import java.net.SocketException
import java.nio.charset.StandardCharsets
import resource._
import scala.util.control.Breaks._

import lambda.traceur.onlinemsg.Msg
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

import scalax.collection.GraphEdge.UnDiEdge

trait Brains {
  def init(numOpponents: Int, graph: R_map) : Unit
  def next(claimed: List[River]) : River
}


class Magic(val numOpponents: Int, val map: R_map) {
  val graph: Graph[SiteId, UnDiEdge] = mapToGraph(map)

  // this thing needs to do the game logic -blinken
  // right now it always attempts to claim (0,1)
  /*def sampleCallback(punter: PunterId, play: HCursor) : T_gameplay = {
    debug("sampleCallback: punter " + punter + " got play: " + play.value.noSpaces);
    debug("sampleCallback: sending move: " + T_gameplay(TR_claim_p(punter, 0, 1)).asJson.noSpaces)
    return T_gameplay(TR_claim_p(punter, 0, 1))
  }*/

  // return a callback function
  def init : (List[River]) => River = {
    return next
  }

  // remove the 
  def next(claimed: List[River]) : River = {
    return claimed.last
  }
}

object LamClient {
  // import resource.ManagedResource
  def send(str: String, out: PrintWriter) : Unit = {
    //debug("send: sending: "+str)
    out.print(str)
    out.flush()
  }

  /* returns empty string at end of stream */
  def receive(in: BufferedInputStream) : String = {
    var rec = ""
    var n = ""
    //debug("receive: receiving")
    breakable { 
    	for( a <- 1 to 10){
        try {
	    	  val c = in.read

          if (c >= '0' && c <= '9') {
            n = n + c.asInstanceOf[Char]
          } else {
            break
          }
        } catch { 
          case e: java.net.SocketException => { 
            debug("receive: got exception " + e)
            System.exit(0)
          }
        }
	    }
	  }
	  if (n == "") {
	    return ""
	  }
	  //debug("receive: reading "+n+" chars")
	  val buffer = new Array[Byte]( n.toInt )
	  val x = in.read(buffer)
    val s = new String(buffer, StandardCharsets.UTF_8)
    //debug("receive: received: "+ s)
    return s
  }

  def buildPacket(json: String) : String = {
    return json.length + ":" + json
  }

  def handleCirceResponse[T >: Null](response: Either[io.circe.Error, T]) : T = {
    response match {
      case Left(msg) => {
        debug("handleCirceResponse: error decoding JSON: " + response)
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
      debug("init: error connecting: got '" + response + "' from server")
      return null
    }

    debug("init: connected. Waiting for other player to join 🍆")

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
  // 
  // returns false if program should exit; true otherwise
  def move(out: PrintWriter, in: BufferedInputStream, punter: PunterId, brains_f: (List[River]) => River) : Boolean = {
    debug("move: waiting for prompt from server")
    val play: Json = handleCirceResponse(parse(receive(in)))
    val cursor: HCursor = play.hcursor

    if (cursor.fieldSet.getOrElse(null).contains("move")) {
      val play_list: HCursor = cursor.downField("move").downField("moves").success.getOrElse(null)
      val next_river: River = brains_f(List[River](River.apply(1, 0))) // static river for now
      val response = buildPacket(T_gameplay(TR_claim_p(punter, next_river.source, next_river.target)).asJson.noSpaces)
      send(response, out)
    } else if (cursor.fieldSet.getOrElse(null).contains("stop")) {
      debug("move: server sent stop message: " + play.noSpaces)
      return false
    } else {
      debug("move: unknown server message: " + play)
      return false
    }

    return true
  }

  // start the game: accepts the streams to communicate with and a callback for
  // brains
  def runGame(out: PrintWriter, in: BufferedInputStream) {
    debug("rungame: initialising")
    val game = init(out, in, false)

    debug("rungame: recieved game: " + game)

    val magic = new Magic(game.setup.punters, game.setup.map)

    // send moves until the server tells us not to
    while (move(out, in, game.setup.punter, magic.next _)) {}

    debug("rungame: shutting down.")
  }

}
