import org.scalatest.{FlatSpec,Matchers}

import java.util.Date
import java.text.SimpleDateFormat
import scala.collection.mutable.HashMap

import lambda.traceur._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.onlinemsg.Msg._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class BrainSpec extends FlatSpec with Matchers {
	val (playerCount, jsonmap) = (16, "samples/nara-sparse.json") 
	val sample = scala.io.Source.fromFile(jsonmap).mkString
	val rmap =  decode[R_map](sample).right.get
	var brain = new MagicBrain()
	var states: HashMap[PunterId, ClaimedEdges] = HashMap()
	for (p: PunterId <- 1 to playerCount) states += (p -> brain.init(p, playerCount, rmap, false))
	val n = (states(1).graph.edges.size / playerCount).asInstanceOf[Int]
	// gameplay logger 
	lambda.traceur.helpers.Helpers.enableLoggingForPunter = 1
    val now = new Date()
    val sdf = new SimpleDateFormat("YYYYMMdd_HHmm_ssSSS")
    lambda.traceur.helpers.Helpers.gameLogFilename = "logs/game_" + sdf.format(now) + ".json" 
    gameLog("{\"setup\":{\"map\":")
    gameLog(rmap.asJson.noSpaces)
    gameLog(",\"punter\":1},\"moves\":[")
	// run the test
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
		gameLog(lambda.traceur.helpers.Helpers.gameLogMoves.mkString(","))
    	gameLog("]}")
		states(1).graph.edges.size should be (0)
	}
}


class PathSpec extends FlatSpec with Matchers {
    val brain = new MagicBrain()
    var state = brain.init(1, 2, loadMap("samples/circle.json"), false)
    for (i <- 0 to 5) {
        val river = brain.nextMove(state)
        state = state.update((1, river) :: Nil)
    }
}
