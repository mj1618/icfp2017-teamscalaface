package lambda
package traceur

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge // probably River should implement this...
import scalax.collection.GraphPredef._

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._ // for R_map, which probably belongs in Types

/* to support the Offline protocol, State implementations should be serialisable to/from JSON */
trait State[Self <: State[Self]] {
	self: Self =>

	/* Note 'newlyClaimed' could be more than one turn's worth if we have timed out.
	 * update must return an instance of the same type (possibly itself, if it is mutable). */
	def update(newlyClaimed: List[(PunterId, River)]) : Self

	/* [Optional] Called when the game is over, to eg. sanity check our expected score against the server's */
	def done(scores: List[(PunterId, Int)]) : Unit = {}
}


/* S is some kind of State type which the brain understands */
trait Brains[S <: State[S]] {
	/* called to initialise the game state */
	def init(me: PunterId, numPlayers: Int, graph: R_map, futuresEnabled: Boolean) : S
	/* returns the next river we should claim */
	def nextMove(state: S) : River
}


/* A simple state which erodes the edges from a Graph as they are claimed */ 
class DecayingGraphState(val graph: SiteGraph) extends State[DecayingGraphState] {
	override def update(claimed: List[(PunterId, River)]) : DecayingGraphState = {
		return new DecayingGraphState(graph -- claimed.map(_._2.edge))
	}
}

