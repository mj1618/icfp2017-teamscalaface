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
import scala.util.control.Breaks._


object BrainHelp {
    implicit val encodeClaimedEdges: Encoder[ClaimedEdges] = new Encoder[ClaimedEdges] {
        final def apply(a: ClaimedEdges): Json = (Json.obj( ("us", a.us.asJson), ("numPlayers", a.numPlayers.asJson), ("mines", a.mines.asJson), ("graph", a.graph.asJson), ("our_graph", a.our_graph.asJson), ("history", a.history.asJson) ))
    }

    implicit val decodeClaimedEdges: Decoder[ClaimedEdges] = new Decoder[ClaimedEdges] {
        final def apply(c: HCursor): Decoder.Result[ClaimedEdges] = for {
          us <- c.downField("us").as[Int]
          numPlayers <- c.downField("numPlayers").as[Int]
          mines <- c.downField("mines").as[List[SiteId]]
          graph <- c.downField("graph").as[SiteGraph]
          our_graph <- c.downField("our_graph").as[SiteGraph]
          history <- c.downField("history").as[List[SiteId]]
        } yield {
          new ClaimedEdges(us, numPlayers, mines, graph, our_graph, None, history)
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
  var graph: SiteGraph, // unclaimed and our edges (routable stuff)
  var our_graph: SiteGraph, // our claimed edges
  var targetRivers: Option[PathType], // where we are going
  var history: List[SiteId] // where we want to travel from
) extends State[ClaimedEdges] {
  
  def this(us: Int, numPlayers: Int, mines: List[SiteId], graph: SiteGraph) {
    this(us, numPlayers, mines, graph, Graph(), None, Nil)
  }

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
    new ClaimedEdges(me, numPlayers, highestValueMines(map.mines, mapToGraph(map)), mapToGraph(map))
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

  def highestValueMines(mines: List[SiteId], graph: Graph[SiteId, UnDiEdge]) : List[SiteId] = {
    var ds = List[Tuple3[Int, Int, Int]]()
    var min = graph.nodes.size+1
    var minL: Tuple3[Int, Int, Int] = null

    ds = for { 
        i <- List.range(0, mines.size)
        j <- List.range(0, mines.size)
        if(i!=j)
      } yield (i, j, shortestPath(mines(i), mines(j), graph))

    ds = ds.sortWith(_._3 < _._3)

    var visited = List[SiteId]()
    var ls = List[R_river]()

    // algo 2
    // breakable{
    //   while(visited.size < mines.size){
    //     var found = false
    //     breakable {
    //       for(d<-ds){
    //         if(!visited.contains(mines(d._1)) && visited.contains(mines(d._2))){
    //           found = true
    //           visited = visited :+ mines(d._1)
    //           break
    //         } else if(visited.contains(mines(d._1)) && !visited.contains(mines(d._2))){
    //           found = true
    //           visited = visited :+ mines(d._2)
    //           break
    //         }
    //       }
    //     }
    //     if(!found){
    //       break
    //     }
    //   }
    // }
    
    // algo 1
    for(d<-ds){
      if(!visited.contains(mines(d._1)) || !visited.contains(mines(d._2))){
        ls = ls :+ R_river(mines(d._1), mines(d._2))
      }
      if(!visited.contains(mines(d._1))){
        visited = visited :+ mines(d._1)
      }
      if(!visited.contains(mines(d._2))){
        visited = visited :+ mines(d._2)
      }
    }
    // (visited, ls)
    visited
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

  def getPath(start: SiteId, targets: List[SiteId], graph: SiteGraph) : Option[PathType] = {
    var paths = List[PathType]()
    var s = graph.get(start)

    for (site <- targets) {
      val path = s.shortestPathTo(graph.get(site))
      if (path != None && path != Nil && path.get.edges.size > 0) {
        paths = path.get :: paths
      }
    }

    if (paths == Nil) {
      var adjacentPath = s.pathUntil(_.outDegree == 1)
      if (adjacentPath != None) paths = adjacentPath.get :: paths
    }
    if (paths == Nil) return None
    return Some(paths.sortWith(_.edges.size < _.edges.size).head)
  }

  override def nextMove(state: ClaimedEdges) : River = {
    val start: SiteId = getStartingPoint(state)
    val mines = getActiveMines(state)
    val path = getPath(start, mines, state.graph)
    debug(s"${state.graph.edges.size} rivers, ${mines.size} mines left, path: $path")
    val claim = path match {
      case None => state.graph.edges.head
      case _ => path.get.edges.head
    }
    return River(claim._1.value, claim._2.value)
  }
}
