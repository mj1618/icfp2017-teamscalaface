import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

import java.io.PrintWriter
import sys.process._
import java.net.{ServerSocket, Socket}
import java.io._
import resource._
import scala.util.control.Breaks._

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
    for( a <- 1 to n.toInt){
    	rec = rec + in.read.asInstanceOf[Char]
    }
    println("received: "+rec)
    return rec
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
    	LamClient.send("16:{\"me\":\"blinken\"}",out)
    	LamClient.receive(in)
    }
  }

}
