package lambda
package traceur

import scala.util.Random
import scalax.collection.GraphEdge.EdgeLike

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._ // for R_map, which probably belongs in Types
import lambda.traceur.helpers.Helpers._

class RandomBrain extends Brains[DecayingGraphState] {
	override def init(me: PunterId, numPlayers: Int, mp: R_map) : DecayingGraphState = {
		return new DecayingGraphState(mapToGraph(mp))
	}

	override def nextMove(state: DecayingGraphState) : River = {
		val e = state.graph.edges.toList
		val edge = e(Random.nextInt(e.size)).value
		River(edge._n(0), edge._n(1))
	}
}
