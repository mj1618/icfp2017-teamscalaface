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

  def play(out: PrintWriter, in: BufferedReader) : Unit = {
  	val game = LamClient.init(out, in)
    println("Recieved game: " + game)
    var g = Graph[SiteId, UnDiEdge]() //[R_site, WUnDiEdge]
    // print
    var r = ""
  	for( r <- game.map.rivers) {
  		g = g + r.source ~ r.target
  	}
  	println("g: "+g)
  }
}

object Application {
  
  def main(args : Array[String]) : Unit = {
  	val dev=true
  	if(dev){
  		
	    var stream = 
	    	"""17:{"you":"blinken"}""" +
	    	"""560:{"punter":1,"punters":2,"map":{"sites":[{"id":4,"x":2.0,"y":-2.0},{"id":1,"x":1.0,"y":0.0},{"id":3,"x":2.0,"y":-1.0},{"id":6,"x":0.0,"y":-2.0},{"id":5,"x":1.0,"y":-2.0},{"id":0,"x":0.0,"y":0.0},{"id":7,"x":0.0,"y":-1.0},{"id":2,"x":2.0,"y":0.0}],"rivers":[{"source":3,"target":4},{"source":0,"target":1},{"source":2,"target":3},{"source":1,"target":3},{"source":5,"target":6},{"source":4,"target":5},{"source":3,"target":5},{"source":6,"target":7},{"source":5,"target":7},{"source":1,"target":7},{"source":0,"target":7},{"source":1,"target":2}],"mines":[1,5]}}"""
	    val out = new PrintWriter(new BufferedWriter(new StringWriter()))
	    // inStream <- managed(new InputStreamReader(connection.getInputStream))
	    val in = new BufferedReader(new InputStreamReader(new ByteArrayInputStream(stream.getBytes)))
	    
	    LamClient.play(out, in)
	  
		} else {
			for { connection <- managed(new Socket("punter.inf.ed.ac.uk", 9006))
		    outStream <- managed(connection.getOutputStream)
		    val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)))
		    inStream <- managed(new InputStreamReader(connection.getInputStream))
		    val in = new BufferedReader(inStream)
		  } {
		    LamClient.play(out, in)
		  }
		}

  
  }

}


object OfflineApplication {
  def main(args: Array[String]) : Unit = {
    println("Hell world")
  }
}
