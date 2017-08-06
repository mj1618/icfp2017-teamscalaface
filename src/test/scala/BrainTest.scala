import org.scalatest.{FlatSpec,Matchers}

import scala.collection.mutable.HashMap

import lambda.traceur._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.onlinemsg.Msg._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class BrainSpec extends FlatSpec with Matchers {
	val (playerCount, jsonmap) = (4, "samples/circle.json") 
	val sample = scala.io.Source.fromFile(jsonmap).mkString
	var brain = new MagicBrain()
	var states: HashMap[PunterId, ClaimedEdges] = HashMap()
	for (p: PunterId <- 1 to playerCount) states += (p -> brain.init(p, playerCount, decode[R_map](sample).right.get, false))
	val n = (states(1).graph.edges.size / playerCount).asInstanceOf[Int]
	it should "pick all edges magically" in {
		for (i <- 0 to n) {
			for ((player, state) <- states if state.graph.edges.size > 0) {
				val move = (player, brain.nextMove(state))
				// apply move to all states
				for ((p, s) <- states) {
					states(p) = states(p).update(move :: Nil)
				}
				debug(s"🚂🚂 $i; 🤓 $player; 📈 ${brain.ourScore(state)}")
			}
		}
		for ((player, state) <- states) {
			debug(s"🕹🕹 🤓 $player; 📊 ${brain.ourScore(state)}")
		}
		states(1).graph.edges.size should be (0)
	}
}
