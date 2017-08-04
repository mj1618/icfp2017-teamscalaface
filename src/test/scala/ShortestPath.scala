import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scala.math.pow

import org.scalatest.{ FlatSpec, Matchers }

class ShortestPathSpec extends FlatSpec with Matchers {
  it should "find shortest path" in {
    println("Hello, world!")
    val lambdamap = Graph (0~1,1~2,0~7,7~6,6~5,5~4,4~3,3~2,1~7,1~3,7~5,5~3)
    val mines = List(1,5)
    for (node <- lambdamap.nodes) {
      var score = pow(lambdamap.edges.size, 2)
      for (mine <- mines) {
        val testscore = pow(node.shortestPathTo(lambdamap.get(mine)).get.edges.size, 2)
        if (testscore < score) score = testscore
      }
      println(s"node $node score $score")
    }
    println("graph " + lambdamap)
  }
}
