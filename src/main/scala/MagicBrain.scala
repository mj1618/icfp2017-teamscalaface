package lambda
package traceur

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scala.math.pow

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._ // for R_map, which probably belongs in Types
import lambda.traceur.helpers.Helpers._

class ClaimedEdges(
  val us: Int,
  val numPlayers: Int,
  val mines: List[SiteId],
  var graph: Graph[SiteId, UnDiEdge] // graph of unclaimed + our claimed edges
) extends State[ClaimedEdges] {

  var our_graph: Graph[SiteId, UnDiEdge] = Graph() // our claimed edges
  //var targetEdges: List[UnDiEdge[SiteId]]
  //var targetNodes: List[SiteId]

  override def update(claimed: List[(PunterId, River)]) : ClaimedEdges = {
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    for ((punter, river) <- claimed) {
      val edge = river.source~river.target
      graph = graph -! edge
      our_graph = punter match {
        case `us` => our_graph + edge
        // also remove foreign-claimed edges from our graph, in case we tried to claim the same edge as someone else and they won
        case _ => our_graph -! edge
      }
    }
    debug(s"next: game graph  o+u: $graph")

    return this
  }
}

class MagicBrain extends Brains[ClaimedEdges] {
  override def init(me: PunterId, numPlayers: Int, map: R_map) : ClaimedEdges = {
    selectTargets(new ClaimedEdges(me, numPlayers, map.mines, mapToGraph(map)))
  }

  // calculate what to claim on map
  def selectTargets(state: ClaimedEdges) : ClaimedEdges = {
    debug(s"There are ${state.mines.size} mines, ${state.graph.edges.size} rivers and ${state.graph.nodes.size} sites in this map.")
    var targetScore = 0.0
    for (startmine <- state.mines) {
      val node = state.graph.get(startmine)
      var score = 0.0
      for (mine <- state.mines) {
        /* val path = node shortestPathTo state.graph.get(mine)
        if (!path.isEmpty) {
          score = score + pow(path.get.edges.size, 2)
        }*/
      }
      println(s"mine $node score $score")
    }
    state
  }

  override def nextMove(state: ClaimedEdges) : River = {
    // sets instead of graphs here
    val unclaimed_edges = state.graph -- (state.our_graph.edges)
    debug("next: unclaimed edges: " + unclaimed_edges)

    // algorithm to pick the best edge goes here
    val edge_to_claim = unclaimed_edges.edges.head
    debug("next: selected edge: " + edge_to_claim)
    return River(edge_to_claim._1, edge_to_claim._2)
  }
}
