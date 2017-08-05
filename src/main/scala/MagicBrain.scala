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
  var our_graph: Graph[SiteId, UnDiEdge] = Graph() // our claimed edges
  var targetRivers: Option[PathType] = None // where we are going
  var history: List[SiteId] = Nil // where we want to travel from

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
        if (history.exists(s => s == src)) history = tgt :: history
        if (history.exists(s => s == tgt)) history = src :: history
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

  def getStartingPoint(state : ClaimedEdges) : Option[SiteId] = {
    val mines = getActiveMines(state)
    val graph = state.graph

    if (state.history != Nil && !graph.nodes.find(state.history.head).isEmpty) {
      return Some(graph.nodes.find(state.history.head).get.value)
    }

    val r = randomFromList(mines)
    if(!r.isEmpty) {
      return r
    }
    var n = state.our_graph.head
    for(n <- state.our_graph.nodes.toList){
      val x = graph.find(n.value)
      return Some(x.get.value)
    }

    return None
  }

  def getPathsToSites(start: SiteId, sites: List[SiteId], graph: Graph[SiteId, UnDiEdge]) : List[PathType] = {
    var shortestpath = graph.edges.size
    var paths = List[PathType]()

    for ( site <- sites ){
      val path = graph.find(start).get.shortestPathTo(graph.find(site).get)
      if (path != None && path != Nil && path.get.edges.size > 0) {
        paths = path.get :: paths
      }
    }
    paths.sortWith(_.edges.size < _.edges.size)
  }

  def getActiveMines(state: ClaimedEdges) : List[SiteId] = {
    val graph = state.graph
    state.mines.filter(graph.find(_) != None)
  }


  // calculate what to claim on map
  def selectTargets(state: ClaimedEdges) : ClaimedEdges = {
    state.targetRivers = None
    val graph = state.graph
    val our_graph = state.our_graph
    debug(s"There are ${state.mines.size} mines, ${graph.edges.size} rivers and ${graph.nodes.size} sites in this map.")
    val mines = getActiveMines(state)
    val randomMine = randomFromList(mines)
    val start = getStartingPoint(state)
    if(!start.isEmpty){
      val paths = getPathsToSites(start.get, mines, graph)
      if(paths.size > 0){
        val path = paths(0)
        state.targetRivers = Some(path)
        println(s"Target path: ${state.targetRivers}")
      }
    }
    
    state
  }

  override def nextMove(state: ClaimedEdges) : River = {
    // sets instead of graphs here
    val graph = state.graph
    val unclaimed_edges = graph -- (state.our_graph.edges)

    // algorithm to pick the best edge (do we need to run every step?)
    selectTargets(state)
    val claim = state.targetRivers match {
      case None => graph.edges.head // fall back on anything
      case _ => state.targetRivers.get.edges.head // otherwise use target path
    }
    debug(s"next: selected edge: $claim")
    return River(claim._1.value, claim._2.value)
  }
}
