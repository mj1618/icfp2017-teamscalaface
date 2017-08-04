import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._

import java.io.PrintWriter
import sys.process._
import org.scalatest.{ FlatSpec, Matchers }

class ShortestPathSpec extends FlatSpec with Matchers {
  it should "find shortest path" in {
    println("Hello, world!")
    val dg = Graph(0~>1, 2~>0, 2~>3, 3~>2, 3~>5, 4~>2,
      4~>3, 5~>4, 6~>0, 6~>4, 6~>9, 7~>6, 7~>8, 8~>7,
      8~>9, 9~>10, 9~>11, 10~>12, 11~>12, 12~>9)
    def n(outer: Int): dg.NodeT = dg get outer
    val path = (n(7) shortestPathTo n(0)).get
    println("graph "+dg)
    println("path "+path)
  }
}
