import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

import java.io.PrintWriter
import sys.process._
import java.net.{ServerSocket, Socket}
import java.io._
import resource._
import scala.util.control.Breaks._

import lambda.traceur.onlinemsg.Msg._

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

  // handshake and get game state
  def init(out: PrintWriter, in: BufferedReader) : Either[io.circe.Error,R_setup] = {

    val hello = T_handshake("blinken").asJson.noSpaces;
    send(buildPacket(hello), out)

    val response = decode[R_handshake](receive(in))
    val game = decode[R_setup](receive(in))

    return game
  }


}

object Application {
  
  def main(args : Array[String]) : Unit = {
    for { connection <- managed(new Socket("punter.inf.ed.ac.uk", 9005))
      outStream <- managed(connection.getOutputStream)
      val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(outStream)))
      inStream <- managed(new InputStreamReader(connection.getInputStream))
      val in = new BufferedReader(inStream)
    } {
      val game = LamClient.init(out, in)
      println("Recieved game: " + game)
    }
  }

}
