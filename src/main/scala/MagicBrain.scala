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

import io.circe._,  io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._


object BrainHelp {
    implicit val encodeClaimedEdges: Encoder[ClaimedEdges] = new Encoder[ClaimedEdges] {
        final def apply(a: ClaimedEdges): Json = (Json.obj( ("us", a.us.asJson), ("numPlayers", a.numPlayers.asJson), ("mines", a.mines.asJson), ("graph", a.graph.asJson) ))
    }

    implicit val decodeClaimedEdges: Decoder[ClaimedEdges] = new Decoder[ClaimedEdges] {
        final def apply(c: HCursor): Decoder.Result[ClaimedEdges] = for {
          us <- c.downField("us").as[Int]
          numPlayers <- c.downField("numPlayers").as[Int]
          mines <- c.downField("mines").as[List[SiteId]]
          graph <- c.downField("graph").as[SiteGraph]
        } yield {
          new ClaimedEdges(us, numPlayers, mines, graph)
        }
    }

    case class OT_setup(ready: PunterId, futures: List[T_future], state: ClaimedEdges)
    case class OR_gameplay(move: R_move, state: ClaimedEdges)
    case class OT_gameplay(claim: TR_claim_p, state: ClaimedEdges)

}

class ClaimedEdges(
  val us: Int,
  val numPlayers: Int,
  val mines: List[SiteId],
  var graph: SiteGraph // unclaimed and our edges (routable stuff)
) extends State[ClaimedEdges] {
  var our_graph: SiteGraph = Graph() // our claimed edges
  var targetRivers: Option[PathType] = None // where we are going
  var history: List[SiteId] = Nil // where we want to travel from

  override def update(claimed: List[(PunterId, River)]) : ClaimedEdges = {
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    for ((punter, river) <- claimed) {
      val edge = river.source~river.target
      graph = graph -! edge
      if (punter == us) {
        our_graph = our_graph + edge
        val (src, tgt) = (river.source.asInstanceOf[SiteId], river.target.asInstanceOf[SiteId])
        if (history == Nil || history.exists(_ == tgt)) history = src :: history
        if (history.exists(_ == src)) history = tgt :: history
      }
    }
    return this
  }
}

class MagicBrain extends Brains[ClaimedEdges] {

  val futuresEnabled = false

  override def init(me: PunterId, numPlayers: Int, map: R_map) : ClaimedEdges = {
    selectTargets(new ClaimedEdges(me, numPlayers, map.mines, mapToGraph(map)))
  }
  
  override def futures(state: ClaimedEdges): List[T_future] = {
    debug("targets: "+state.targetRivers)
    debug("mines: "+state.mines)
    if(futuresEnabled){
      val futures = state.targetRivers match {
        case None => List()
        case Some(path) => for (p<-path.edges.toList)
            yield T_future(path.edges.toList.head._1.value, p._2.value)
      }
      val fs = futures.take(5).distinct
      debug("futures: "+fs)
      fs
    } else {
      List()
    }
  }

  def getActiveMines(state: ClaimedEdges) : List[SiteId] = {
    // return mines we haven't touched in the active graph
    val graph = state.graph
    state.mines.filter(mine => state.our_graph.find(mine) == None && graph.find(mine) != None)
  }

  def getStartingPoint(state : ClaimedEdges) : SiteId = {
    // #. Pick mine with highest starting value (assume state.mines is head to tail best to worst)
    if (state.history == Nil) return state.mines.head
    // #. Pick most recently visited site from history with a path to next most valuable disconnected mine
    val graph = state.graph
    for (mine <- getActiveMines(state)) {
      for (site <- state.history if graph.find(site) != None) {
        if (!graph.get(site).shortestPathTo(graph.get(mine)).isEmpty) return site
      }
    }
    // #. Pick most recently visited site from history with an available river
    for (site <- state.history if graph.find(site) != None) return site
    // #. give up and pick anything
    return graph.nodes.head.value
  }

  def getPathsToSites(start: SiteId, sites: List[SiteId], graph: SiteGraph) : List[PathType] = {
    var paths = List[PathType]()
    var s = graph.get(start)

    for (site <- sites) {
      val path = s.shortestPathTo(graph.get(site))
      if (path != None && path != Nil && path.get.edges.size > 0) {
        paths = path.get :: paths
      }
    }

    if (paths == Nil) {
      var adjacentPath = s.pathUntil(_.outDegree == 1)
      if (adjacentPath != None) paths = adjacentPath.get :: paths
    }
    return paths.sortWith(_.edges.size < _.edges.size)
  }

  // calculate what to claim on map
  def selectTargets(state: ClaimedEdges) : ClaimedEdges = {
    val start: SiteId = getStartingPoint(state)
    val mines = getActiveMines(state)
    val paths = getPathsToSites(start, mines, state.graph)
    state.targetRivers = None
    if (paths.size > 0) state.targetRivers = Some(paths(0))
    debug(s"${state.graph.edges.size} rivers, ${mines.size} mines left, path: ${state.targetRivers}")
    return state
  }

  override def nextMove(state: ClaimedEdges) : River = {
    selectTargets(state)
    val claim = state.targetRivers match {
      case None => state.graph.edges.head // fall back on anything
      case _ => state.targetRivers.get.edges.head // otherwise use target path
    }
    return River(claim._1.value, claim._2.value)
  }
}
