import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scala.math.pow

import org.scalatest.{ FlatSpec, Matchers }

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.helpers.Helpers._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import scala.io.Source.fromFile

class ShortestPathSpec extends FlatSpec with Matchers {
  it should "find shortest path" in {
    val rmap = loadMap("samples/gothenburg-sparse.json")
    val graph = mapToGraph(rmap)
    println(s"There are ${rmap.mines.size} mines, ${graph.edges.size} rivers and ${graph.nodes.size} sites in this map.")
    val mines = idsToSites(rmap.mines)
    for (startmine <- mines) {
      val node = graph.get(startmine)
      var score = 0.0
      for (mine <- mines) {
        val path = node.shortestPathTo(graph.get(mine))
        if (!path.isEmpty) {
          score = score + pow(path.get.edges.size, 2)
        }
      }
      println(s"mine $node score $score")
    }/*
    println(graph.toDot(DotRootGraph(
      directed  = false, 
      id        = Some(Id("MyDot")),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr(Id("shape"), Id("record")))))
    ), _ => None))
    */
  }
}
