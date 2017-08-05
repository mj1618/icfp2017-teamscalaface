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
  var graph: Graph[SiteId, UnDiEdge] // unclaimed and our edges (routable stuff)
) extends State[ClaimedEdges] {
  type PathType = Graph[SiteId, UnDiEdge]#Path
  var our_graph: Graph[SiteId, UnDiEdge] = Graph() // our claimed edges
  var targetRivers: Option[PathType] = None // where we are going
  var activeSites: List[SiteId] = Nil // where we want to travel from

  override def update(claimed: List[(PunterId, River)]) : ClaimedEdges = {
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    for ((punter, river) <- claimed) {
      val edge = river.source~river.target
      if (punter == us) {
        graph = graph -! edge
        our_graph = our_graph + edge
        val (src, tgt) = (river.source.asInstanceOf[SiteId], river.target.asInstanceOf[SiteId])
        if (activeSites.exists(s => s == src)) activeSites = tgt :: activeSites
        if (activeSites.exists(s => s == tgt)) activeSites = src :: activeSites
      } else {
        graph = graph -! edge
      }
    }
    return this
  }
}

class MagicBrain extends Brains[ClaimedEdges] {
  override def init(me: PunterId, numPlayers: Int, map: R_map) : ClaimedEdges = {
    selectTargets(new ClaimedEdges(me, numPlayers, map.mines, mapToGraph(map)))
    // futures bets here
  }
  
  override def futures(state: ClaimedEdges): List[T_future] = {
    debug("targets: "+state.targetRivers)
    debug("mines: "+state.mines)
    val futures = state.targetRivers match {
      case None => List()
      case Some(path) => for (p<-path.edges.toList)
          yield T_future(path.edges.toList.head._1.value, p._2.value)
    }
    futures.take(5)
  }

  // calculate what to claim on map
  def selectTargets(state: ClaimedEdges) : ClaimedEdges = {
    state.targetRivers = None
    val graph = state.graph
    val our_graph = state.our_graph
    debug(s"There are ${state.mines.size} mines, ${graph.edges.size} rivers and ${graph.nodes.size} sites in this map.")
    val mines = state.mines.filter(graph.find(_) != None)
    // skip calc if no mines reachable
    // in future this should try to make chain longer
    if (mines != Nil) {
      var start = graph.find(mines(Random.nextInt(mines.size)))
      if (state.activeSites != Nil) start = graph.find(state.activeSites.head)
      debug(s"start: $start")
      //
      if (start == None) {
        start = graph.find(our_graph.nodes.head.value)
      }
      val s = graph.get(start.get.value)
      var shortestpath = graph.edges.size
      for (mine <- mines) {
        val path = s.shortestPathTo(graph.get(mine))
        if (path != None && path.get.edges.size < shortestpath) {
          state.targetRivers = path
        }
      }
    }
    println(s"Target path: ${state.targetRivers}")
    state
  }

  override def nextMove(state: ClaimedEdges) : River = {
    // sets instead of graphs here
    val unclaimed_edges = state.graph -- (state.our_graph.edges)

    // algorithm to pick the best edge (do we need to run every step?)
    selectTargets(state)
    val claim = state.targetRivers match {
      case None => state.graph.edges.head // fall back on anything
      case _ => state.targetRivers.get.edges.head // otherwise use target path
    }
    debug(s"next: selected edge: $claim")
    return River(claim._1.value, claim._2.value)
  }
}
