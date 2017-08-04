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

class Magic(val numOpponents: Int, val map: R_map) {
  // graph of unclaimed + our claimed edges
  var graph: Graph[SiteId, UnDiEdge] = mapToGraph(map)
  // our claimed edges
  var our_graph: Graph[SiteId, UnDiEdge] = Graph()

  // given a list of rivers claimed this turn, calculate the next river to
  // claim. Rivers we claim are excluded from the claimed-list
  def next(claimed_rivers: List[TR_claim_p]) : River = {
    println("magic::next: got claimed-list " + claimed_rivers.mkString(", "))
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    for (r <- claimed_rivers) { graph = graph -! r.source~r.target }
    for (r <- claimed_rivers) { our_graph = our_graph -! r.source~r.target } // also remove foreign-claimed edges from our graph, in case we tried to claim the same edge as someone else and they won
    println("next: game graph  o+u: " + graph)

    // sets instead of graphs here
    val unclaimed_edges = graph -- (our_graph.edges)
    println("next: unclaimed edges: " + unclaimed_edges)

    // algorithm to pick the best edge goes here
    val edge_to_claim = unclaimed_edges.edges.head
    println("next: selected edge: " + edge_to_claim)

    our_graph = our_graph + edge_to_claim // fixme possibly this could be done without destroying the objecT
    return River(edge_to_claim._1, edge_to_claim._2)
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
  def move(out: PrintWriter, in: BufferedInputStream, punter: PunterId, brains_f: (List[TR_claim_p]) => River) : Boolean = {
    debug("move: waiting for prompt from server")
    val play: Json = handleCirceResponse(parse(receive(in)))
    val cursor: HCursor = play.hcursor

    if (cursor.fieldSet.getOrElse(null).contains("move")) {
      //val play_list: HCursor = cursor.downField("move").downField("moves").success.getOrElse(null)
      val play_list: HCursor = cursor.downField("move").downField("moves").success.getOrElse(null)
      var i = 0
      var river_claim_list: List[TR_claim_p] = List() // fixme, this is O(n) for appends, should use ListBuffer? do we care?

      // here, I attempt to convert scala into ruby by sheer force of will
      while (play_list.downArray.rightN(i).fields != None) { // fixme, can we iterate over this?
        play_list.downArray.rightN(i).fields.getOrElse(null).last match {
          case "pass" => { 
            val response = handleCirceResponse(decode[TR_punter]( play_list.downArray.rightN(i).downField("pass").success.getOrElse(null).value.noSpaces)) 
            debug("move: punter " + response.punter + (if (response.punter == punter) " (me!)" else "") + " passed this turn")
          }
          case "claim" => { 
            val response = handleCirceResponse(decode[TR_claim_p]( play_list.downArray.rightN(i).downField("claim").success.getOrElse(null).value.noSpaces)) 
            debug("move: punter " + response.punter + (if (response.punter == punter) " (me!)" else "") + " claimed river (" + response.source + "," + response.target + ")")
            // don't include rivers we claim in the claimed list
            if (response.punter != punter) {
              river_claim_list = river_claim_list ::: List(TR_claim_p(response.punter, response.source, response.target))
            }
          }
        }
        i += 1
      }

      // send the callback the list of rivers claimed and get the next move back
      val next_river: River = brains_f(river_claim_list)
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
