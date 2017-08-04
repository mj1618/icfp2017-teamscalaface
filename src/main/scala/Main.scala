import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

import java.io.PrintWriter
import sys.process._

object Main extends App {
  println("16:{\"me\":\"blinken\"}")

  // Simple client
  import java.net._
  import java.io._
  import scala.io._

  val s = new Socket(InetAddress.getByName("punter.inf.ed.ac.uk"), 9006)
  //val s = new Socket(InetAddress.getByName("canireachthe.cloud"), 80)
  val is = new BufferedSource(s.getInputStream())
  //val is = s.getInputStream()
  val out = new PrintStream(s.getOutputStream())

  out.println("16:{\"me\":\"blinken\"}")
  out.flush()
  Thread sleep 1000

  while(is.hasNext)
    print(is.next())

  s.close()
}
