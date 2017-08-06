import org.scalatest.{FlatSpec,Matchers}

import scala.collection.mutable.HashMap

import lambda.traceur._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.onlinemsg.Msg._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class BrainSpec extends FlatSpec with Matchers {
	val sample = scala.io.Source.fromFile("samples/circle.json").mkString
	var brain = new MagicBrain()
	val (p1, p2, p3, p4) : (PunterId, PunterId, PunterId, PunterId) = (1, 2, 3, 4)
	var states: HashMap[PunterId, ClaimedEdges] = HashMap()
	for (p <- List(p1, p2, p3, p4)) states += (p -> brain.init(p, 4, decode[R_map](sample).right.get))
	val n = (states(p1).graph.edges.size / 4).asInstanceOf[Int]
	it should "pick all edges magically" in {
		for (i <- 0 to n) {
			for ((player, state) <- states if state.graph.edges.size > 0) {
				val move = (player, brain.nextMove(state))
				// apply move to all states
				for ((p, s) <- states) {
					states(p) = states(p).update(move :: Nil)
					debug(s"move $i; player $p; score ${brain.ourScore(s)}")
				}
			}
		}
		states(p1).graph.edges.size should be (0)
	}
}
