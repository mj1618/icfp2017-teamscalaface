package lambda
package traceur
package lamclient

import java.io.PrintWriter
import sys.process._
import java.io._
import java.net.SocketException
import java.nio.charset.StandardCharsets
import resource._
import scala.util.control.Breaks._

import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.BrainHelp._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

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
            //debug("receive: got exception " + e)
            System.exit(0)
          }
        }
	    }
	  }
	  if (n == "") {
	    return ""
	  }
	  // debug("receive: reading "+n+" chars")
    var nread = 0
    var s = ""
    val buffer = new Array[Byte]( n.toInt )
    while(nread < n.toInt-1){
      //debug("reading")

      val x = in.read(buffer, 0, n.toInt - nread)
      s = s + new String(buffer.slice(0,x), StandardCharsets.UTF_8)
      nread += x

      //debug("read loop:"+nread)
    }

     //debug("receive: received: "+ s.length + " "+s)
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

  def handshake(out: PrintWriter, in: BufferedInputStream) {
    val name = "blinken"
    val hello = buildPacket(T_handshake(name).asJson.noSpaces);
    send(hello, out)
    //debug("handshake: sent '" + hello + "'")
    val response = handleCirceResponse(decode[R_handshake](receive(in)))

    if (response.you != name) {
      debug("handshake: uh oh, got '" + response + "' from server")
      System.exit(1)
    }
  }

  // get initial game state
  def init[S <: State[S]](out: PrintWriter, setup: R_setup, brains: MagicBrain, offline: Boolean = false) : (ClaimedEdges) = {

    // waiting for this may take some time
    val game = brains.init(setup.punter, setup.punters, setup.map)

    val futuresList = brains.futures(game)
    
    if (offline) {
      val ready = buildPacket(OT_setup(setup.punter, futuresList, game).asJson.noSpaces);
      send(ready, out)
    } else {
      val ready = buildPacket(T_setup(setup.punter, futuresList).asJson.noSpaces);
      send(ready, out)
    }

    game
  }

  def parseClaims(punter: PunterId, list: HCursor) : List[(PunterId, River)] = {
    var river_claim_list: List[(PunterId, River)] = List() // fixme, this is O(n) for appends, should use ListBuffer? do we care?
    var i = 0

    // here, I attempt to convert scala into ruby by sheer force of will
    while (list.downArray.rightN(i).fields != None) { // fixme, can we iterate over this?
      list.downArray.rightN(i).fields.getOrElse(null).last match {
        case "pass" => {
          val response = handleCirceResponse(decode[TR_punter]( list.downArray.rightN(i).downField("pass").success.getOrElse(null).value.noSpaces))
          debug("move: punter " + response.punter + (if (response.punter == punter) " (me!)" else "") + " passed this turn")
        }
        case "claim" => {
          val response = handleCirceResponse(decode[TR_claim_p]( list.downArray.rightN(i).downField("claim").success.getOrElse(null).value.noSpaces))
          debug("move: punter " + response.punter + (if (response.punter == punter) " (me!)" else "") + " claimed river (" + response.source + "," + response.target + ")")
          river_claim_list = river_claim_list :+ (response.punter, River(response.source, response.target))
        }
      }
      i += 1
    }
    river_claim_list
  }

  // waits for R_gameplay from the server, sends it to a callback, and sends
  // the server back the callback's response. Callback must accept HCursor (a
  // cursor to a JSON list) and return JSON string
  // 
  // returns false if program should exit; true otherwise
  def moveWrap[S <: State[S]](out: PrintWriter, in: BufferedInputStream, punter: PunterId, brains: MagicBrain, initialState: ClaimedEdges, offline: Boolean = false) : Boolean = {
    val play: Json = handleCirceResponse(parse(receive(in)))
    move(out, play, punter, brains, initialState, offline)
  }


  def move[S <: State[S]](out: PrintWriter, play: Json, punter: PunterId, brains: MagicBrain, initialState: ClaimedEdges, offline: Boolean = false) : Boolean = {
    debug("move: waiting for prompt from server")
    val cursor: HCursor = play.hcursor
    var state = initialState

    if (cursor.fieldSet.getOrElse(null).contains("move")) {
      //val play_list: HCursor = cursor.downField("move").downField("moves").success.getOrElse(null)
      val play_list: HCursor = cursor.downField("move").downField("moves").success.getOrElse(null)

      // send the callback the list of rivers claimed and get the next move back
      state = state.update(parseClaims(punter, play_list))
      val next_river: River = brains.nextMove(state)
      if (offline) {
        val response = buildPacket(OT_gameplay(TR_claim_p(punter, next_river.source, next_river.target), state).asJson.noSpaces)
        send(response, out)

      } else {
        val response = buildPacket(T_gameplay(TR_claim_p(punter, next_river.source, next_river.target)).asJson.noSpaces)
        send(response, out)
      }

    } else if (cursor.fieldSet.getOrElse(null).contains("stop")) {
      val play_list: HCursor = cursor.downField("stop").downField("moves").success.getOrElse(null)
      state = state.update(parseClaims(punter, play_list))
      def elem2score: io.circe.Json => (PunterId, Int) = (x) => {
        val cur = x.hcursor
        (cur.get[PunterId]("punter").getOrElse(-1), cur.get[Int]("score").getOrElse(-1))
      }
      val scores = cursor.downField("stop").downField("scores").focus.get.asArray.getOrElse(Vector.empty).map(elem2score)
      state.done(scores.toList)
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
  def runGame[S <: State[S]](out: PrintWriter, in: BufferedInputStream, brains: MagicBrain) {
    debug("runGame: talking smack")
    val shake = handshake(out, in)

    debug("runGame: all G. Waiting for other players to join 🍆")
    val setup = handleCirceResponse(decode[R_setup](receive(in)))
    val game = init(out, setup, brains, false)


    // debug("STATE DUMP AHEAD")
    //debug(setup.asJson)
    //debug(game.asJson)

    // send moves until the server tells us not to
    while (moveWrap(out, in, setup.punter, brains, game, false)) {}

    debug("runGame: we're done here")
  }

  def runGameOffline[S <: State[S]](out: PrintWriter, in: BufferedInputStream, brains: MagicBrain) {
    debug("runGameOffline: talking smack")
    val shake = handshake(out, in)

    val message = handleCirceResponse(parse(receive(in)))
    if (message.hcursor.fieldSet.getOrElse(null).contains("punter")) {
      debug("runGameOffline: setup message detected!")
      val setup = handleCirceResponse(message.as[R_setup])
      val game = init(out, setup, brains, true)
    } else {
      debug("runGameOffline: moves message detected!")
      val game = handleCirceResponse(message.hcursor.downField("state").as[ClaimedEdges])
      move(out, message, game.us, brains, game, true)
    }
    
    debug("runGameOffline: ahh, the biiiiig sleep")
  }
}
