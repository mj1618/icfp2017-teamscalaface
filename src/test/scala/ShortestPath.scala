import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scala.math.pow

import org.scalatest.{ FlatSpec, Matchers }

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.helpers.Helpers.mapToGraph
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import scala.io.Source.fromFile

class ShortestPathSpec extends FlatSpec with Matchers {
  it should "find shortest path" in {
    println("Parse a file!")
    val data = decode[R_map](fromFile("samples/gothenburg-sparse.json").mkString).right.get
    val lambdamap = mapToGraph(data)
    val mines = data.mines
    println(s"There are ${mines.size} mines, ${lambdamap.edges.size} rivers and ${lambdamap.nodes.size} sites in this map.")
    for (startmine <- mines) {
      val node = lambdamap.get(startmine)
      var score = 0.0
      for (mine <- mines) {
        val path = node.shortestPathTo(lambdamap.get(mine))
        if (!path.isEmpty) {
          score = score + pow(path.get.edges.size, 2)
        }
      }
      println(s"mine $node score $score")
    }/*
    println(lambdamap.toDot(DotRootGraph(
      directed  = false, 
      id        = Some(Id("MyDot")),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr(Id("shape"), Id("record")))))
    ), _ => None))
    */
  }
}
