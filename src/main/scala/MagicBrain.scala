package lambda
package traceur

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._ // for R_map, which probably belongs in Types
import lambda.traceur.helpers.Helpers._

class ClaimedEdges(
  val us: Int,
  val numPlayers: Int,
  var graph: Graph[SiteId, UnDiEdge] // graph of unclaimed + our claimed edges
) extends State[ClaimedEdges] {

  var our_graph: Graph[SiteId, UnDiEdge] = Graph() // our claimed edges

  override def update(claimed: List[(PunterId, River)]) : ClaimedEdges = {
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    for (r <- claimed) {
      val edge = r._2.source~r._2.target
      graph = graph -! edge
      if (r._1 == us) {
        our_graph = our_graph + edge
      } else {
        // also remove foreign-claimed edges from our graph, in case we tried to claim the same edge as someone else and they won
        our_graph = our_graph -! edge
      }
    }
    debug("next: game graph  o+u: " + graph)

    return this
  }
}

class MagicBrain extends Brains[ClaimedEdges] {
  override def init(me: PunterId, numPlayers: Int, map: R_map) : ClaimedEdges = {
    new ClaimedEdges(me, numPlayers, mapToGraph(map))
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
