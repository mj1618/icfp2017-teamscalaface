package lambda
package traceur

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scala.math.pow
import scala.util.Random

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._ // for R_map, which probably belongs in Types
import lambda.traceur.helpers.Helpers._

class ClaimedEdges(
  val us: Int,
  val numPlayers: Int,
  val mines: List[SiteId],
  var graph: Graph[SiteId, UnDiEdge] // graph of unclaimed + our claimed edges
) extends State[ClaimedEdges] {
  val graphType: Graph[SiteId, UnDiEdge] = Graph() // static graph obj for stealing types from
  var our_graph: Graph[SiteId, UnDiEdge] = Graph() // our claimed edges
  var targetRivers: List[(Double, graphType.Path)] = Nil
  var targetSites: List[(Double, SiteId, SiteId)] = Nil

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
    return this
  }
}

class MagicBrain extends Brains[ClaimedEdges] {
  override def init(me: PunterId, numPlayers: Int, map: R_map) : ClaimedEdges = {
    selectTargets(new ClaimedEdges(me, numPlayers, map.mines, mapToGraph(map)))
  }

  // calculate what to claim on map
  def selectTargets(state: ClaimedEdges) : ClaimedEdges = {
    state.targetRivers = Nil
    state.targetSites = Nil
    val graph = state.graph
    debug(s"There are ${state.mines.size} mines, ${graph.edges.size} rivers and ${graph.nodes.size} sites in this map.")
    val firstMine = state.mines.find(graph.find(_) != None)
    // skip calc if no mines
    if (firstMine != None) {
      // guess how long each path calc takes
      val t0 = System.nanoTime()
      val n = graph.nodes.toList
      val mine = state
      for (i <- 1 to 6) graph.get(firstMine.get).shortestPathTo(n(Random.nextInt(n.size)))
      // keep 1/3rd of a sec for other stuff
      val steps = pow(10, 9) * 4 / (System.nanoTime() - t0)
      debug(s"Time for $steps path calcs!")
      val minesteps = (steps/state.mines.size).asInstanceOf[Int]
      val checknodes = graph.nodes.takeRight(minesteps)
      var targetScore = 0.0
      // outer loop mines
      for (startmine <- state.mines if graph.find(startmine) != None) {
        val node = graph.get(startmine)
        var score = 0.0
        // inner loop set of nodes we can do in less than a sec
        for (site <- checknodes) {
          val path = node.shortestPathTo(site)
          if (!path.isEmpty) {
            score = score + pow(path.get.edges.size, 2)
            if (score > targetScore) {
              // should end up with paths from mines going to best scored nodes
              state.targetRivers = (score, path.get.asInstanceOf[state.graphType.Path]) :: state.targetRivers
              state.targetSites = (score, startmine, site.value) :: state.targetSites
              targetScore = score
            }
          }
        }
      }
    }
    println(s"Targeting these sites ${state.targetSites}")
    state
  }

  override def nextMove(state: ClaimedEdges) : River = {
    // sets instead of graphs here
    val unclaimed_edges = state.graph -- (state.our_graph.edges)

    // algorithm to pick the best edge (do we need to run every step?)
    selectTargets(state)
    val claim = state.targetRivers match {
      case Nil => state.graph.edges.head // fall back on anything
      case (score, path) :: _ => path.edges.head // otherwise use target path
    }
    debug(s"next: selected edge: $claim")
    return River(claim._1.value, claim._2.value)
  }
}
