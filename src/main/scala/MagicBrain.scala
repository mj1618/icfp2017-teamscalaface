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
        final def apply(a: ClaimedEdges): Json = (Json.obj( ("us", a.us.asJson), ("numPlayers", a.numPlayers.asJson), ("mines", a.mines.asJson), ("graph", a.graph.asJson), ("our_graph", a.our_graph.asJson), ("history", a.history.asJson) ))
    }

    implicit val decodeClaimedEdges: Decoder[ClaimedEdges] = new Decoder[ClaimedEdges] {
        final def apply(c: HCursor): Decoder.Result[ClaimedEdges] = for {
          us <- c.downField("us").as[Int]
          numPlayers <- c.downField("numPlayers").as[Int]
          mines <- c.downField("mines").as[List[SiteId]]
          futures <- c.downField("futures").as[List[T_future]]
          graph <- c.downField("graph").as[SiteGraph]
          our_graph <- c.downField("our_graph").as[SiteGraph]
          history <- c.downField("history").as[List[SiteId]]
        } yield {
          new ClaimedEdges(us, numPlayers, idsToSites(mines), futures, graph, our_graph, idsToSites(history))
        }
    }

    case class OT_setup(ready: PunterId, futures: List[T_future], state: ClaimedEdges)
    case class OR_gameplay(move: R_move, state: ClaimedEdges)
    case class OT_gameplay(claim: TR_claim_p, state: ClaimedEdges)

}

class ClaimedEdges(
  val us: Int,
  val numPlayers: Int,
  val mines: List[Site],
  val futures: List[T_future],
  var graph: SiteGraph, // unclaimed edges (routable stuff)
  var our_graph: SiteGraph, // our claimed edges
  var history: List[Site] // where we want to travel from
) extends State[ClaimedEdges] {
  var last_move: River = null
  var game_graph: SiteGraph = graph // the whole game. graph objects are immutable so ok to pass reference
  
  def this(us: Int, numPlayers: Int, mines: List[Site], futures: List[T_future], graph: SiteGraph) {
    this(us, numPlayers, mines, futures, graph, Graph(), Nil)
  }

  override def update(claimed: List[(PunterId, River)]) : ClaimedEdges = {
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    for ((punter, river) <- claimed) {
      val edge = river.edge
      graph = graph -! edge
      if (punter == us) {
        our_graph = our_graph + edge
        val (src, tgt) = (Site(river.source), Site(river.target))
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
    val graph = mapToGraph(map)
    val mineSites = idsToSites(map.mines)
    val mines = getMinesLongest(mineSites, graph)
    val futures = getFutures(mines, graph)
    new ClaimedEdges(me, numPlayers, mineSites, futures, graph)
  }
  
  def getFutures(mines: List[Site], graph: SiteGraph): List[T_future] = {
    if(futuresEnabled && mines.size > 1){
      val futures = shortestPath(mines(0), mines(1), graph) match {
        case None => List()
        case Some(path) => for (p <- path.edges.toList if !mines.contains(p))
            yield T_future(mines(0), p._2.value)
      }
      val fs = futures.distinct
      debug("futures: "+fs)
      fs
    } else {
      List()
    }
  }

  def getAllDistances(mines: List[Site], graph: SiteGraph) : List[Tuple3[Int, Int, Int]] = {
    var ds = for { 
      i <- List.range(0, mines.size)
      j <- List.range(0, mines.size)
      if(i != j && i < j && shortestPathSize(mines(i), mines(j), graph)!=0)
    } yield (i, j, shortestPathSize(mines(i), mines(j), graph))
    ds.sortWith(_._3 < _._3)
  }

  def getMinesRandom(mines: List[SiteId], graph: Graph[SiteId, UnDiEdge]) : List[SiteId] = {
    scala.util.Random.shuffle(mines)
  }

  // This get's the fastest path around all the mines.
  // It won't necessarily grab a lot of mines early on though
  def getMinesLongest(mines: List[Site], graph: SiteGraph) : List[Site] = {

    if(mines.size<=2){
      return mines
    }

    var ds = getAllDistances(mines, graph)
    var visited = List[Int]() // list of indices

    visited = visited :+ ds(0)._1

    while(visited.size < mines.size){
      val od = ds.find( dl => (visited.contains(dl._1) && !visited.contains(dl._2)) || (!visited.contains(dl._1) && visited.contains(dl._2)))
      od match {
        case None => {
          visited = visited :+ ds(0)._1
        }
        case Some(d) => {
          if(!visited.contains(d._1)){
            visited = visited :+ d._1
          } else {
            visited = visited :+ d._2
          }
          ds = ds.filter( dl => !((dl._1==d._1 && dl._2==d._2) || (dl._1==d._2 && dl._2==d._1)))
        }
      }
    }
    visited.map(v=>mines(v))
  }

  // ensures you will collect as many mines as fast as possible
  // does to in a disconnected way. I.e. not following 1 path the whole time
  def getMinesFastest(mines: List[Site], graph: SiteGraph) : List[Site] = {
    var ds = getAllDistances(mines, graph)
    var visited = List[Int]() // list of indices
    var ls = List[R_river]()

    for(d<-ds){
      if(!visited.contains(d._1)){
        visited = visited :+ d._1
      }
      if(!visited.contains(d._2)){
        visited = visited :+ d._2
      }
    }
    visited.map(v=>mines(v))
  }

  def getTargetSites(state: ClaimedEdges) : List[Site] = {
    // return reachable mines we haven't yet connected to our active graph
    val graph = state.graph
    state.futures.filter(future => state.our_graph.find(Site(future.target)) == None && graph.find(Site(future.target)) != None).map(f=>Site(f.target)) ::: state.mines.filter(mine => state.our_graph.find(mine) == None && graph.find(mine) != None)
  }

  // xx for now, assume we have one connected graph
  // get all sites that we have claimed
  // get all mines we have claimed
  // calculate shortest path from all claimed mines to all claimed sites, *using full game graph*
  // score is sum(d^2)

  // https://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    debug("time: ^ took " + (t1 - t0)/1000000 + "ms")
    result
  }

  def ourScore(state: ClaimedEdges) : Int = {
    var score: Int = 0;
    var mm: List[Site] = state.mines;
    time {
      for (mine <- state.mines) {
        val our_graph: SiteGraph = state.our_graph
        val game_graph: SiteGraph = state.game_graph
        game_graph.find(mine) match {
          case None => {}
          case Some(mine_node) => {
            // looping over all mines
            for (site <- state.our_graph.nodes.toList) {
              val site_i: Site = site.value
              val path = mine_node.shortestPathTo(game_graph.find(site_i).get)
              val length: Int = path match {
                case None => 0 // sites are disconnected
                case Some(p) => p.edges.size
              }
              score += length * length
              //debug("ourscore: mine " + mine + " to site " + site + " has shortest path " + length + ", cumulative score " + score)
            }
          }
        }
      }
      //debug("ourscore: our graph: " + state.our_graph.mkString(" "))
      //debug("ourscore: game graph: " + state.game_graph.mkString(" "))
      debug("ourscore: total score " + score)
    }
     
    return score
  }


  def getStartingPoint(state : ClaimedEdges) : Site = {
    // #. Pick mine with highest starting value (assume state.mines is head to tail best to worst)
    if (state.history == Nil) return state.mines.head
    // #. Pick most recently visited site from history with a path to next most valuable disconnected mine
    val graph = state.graph
    for (mine <- getTargetSites(state)) {
      for (site <- state.history if graph.find(site) != None) {
        if (!graph.get(site).shortestPathTo(graph.get(mine)).isEmpty) return site
      }
    }
    // #. Pick most recently visited site from history with an available river
    for (site <- state.history if graph.find(site) != None) return site
    // #. give up and pick anything
    return graph.nodes.head.value
  }

  def getPath(start: Site, targets: List[Site], graph: SiteGraph) : Option[PathType] = {
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
    val start: Site = getStartingPoint(state)
    val mines = getTargetSites(state)
    val path = getPath(start, mines, state.graph)
    debug(s"${state.graph.edges.size} rivers, ${mines.size} mines left, path: $path")
    val claim = path match {
      case None => state.graph.edges.head
      case _ => path.get.edges.head
    }

    val next_river: River = River(claim._1.value, claim._2.value)
    state.last_move = next_river

    return next_river
  }

}
