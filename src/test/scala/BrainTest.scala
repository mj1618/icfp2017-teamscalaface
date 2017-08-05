import org.scalatest.{FlatSpec,Matchers}

import lambda.traceur._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.onlinemsg.Msg._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class BrainSpec extends FlatSpec with Matchers {
	val me : PunterId = 1
	val sample = scala.io.Source.fromFile("samples/circle.json").mkString

	it should "pick all edges randomly" in {
		var brain = new RandomBrain() /*
		var state = brain.init(me, 2, decode[R_map](sample).right.get)
		val n = state.graph.edges.size
		for (i <- 1 to n) {
			val river = brain.nextMove(state)
			debug(s"claiming $river")
			state = state.update((me, river) :: Nil)
		}
		state.graph.edges.size should be (0)
		*/
	}

	it should "pick all edges magically" in {
		var brain = new MagicBrain()
		var state = brain.init(me, 2, decode[R_map](sample).right.get)
		val n = state.graph.edges.size
		for (i <- 1 to n) {
			val river = brain.nextMove(state)
			debug(s"claiming $river")
			state = state.update((me, river) :: Nil)
		}
		state.graph.edges.size should be (0)
	}
}
