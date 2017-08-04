import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.io.dot._
import scala.math.pow

import org.scalatest.{ FlatSpec, Matchers }

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import scala.io.Source.fromFile

class ShortestPathSpec extends FlatSpec with Matchers {
  it should "find shortest path" in {
    println("Parse a file!")
    val data = decode[R_map](fromFile("samples/gothenburg-sparse.json").mkString).right.get
    val lambdamap = Graph.from(for (site <- data.sites) yield site.id, for (river <- data.rivers) yield river.source ~ river.target) 
    val mines = data.mines
    for (node <- lambdamap.nodes) {
      var score = 0.0
      for (mine <- mines) {
        val path = node.shortestPathTo(lambdamap.get(mine))
        if (!path.isEmpty) {
          score = score + pow(path.get.edges.size, 2)
        }
      }
      println(s"node $node score $score")
    }/*
    println(lambdamap.toDot(DotRootGraph(
      directed  = false, 
      id        = Some(Id("MyDot")),
      attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr(Id("shape"), Id("record")))))
    ), _ => None))
    */
  }
}
