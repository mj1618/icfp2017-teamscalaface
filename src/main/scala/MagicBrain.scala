package lambda
package traceur

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.GraphTraversal.BreadthFirst
import scala.math.pow
import scala.util.Random

import lambda.traceur.Types._
import lambda.traceur.onlinemsg.Msg._ // for R_map, which probably belongs in Types
import lambda.traceur.helpers.Helpers._

import io.circe._,  io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object BrainHelp {
    implicit val encodeClaimedEdges: Encoder[ClaimedEdges] = new Encoder[ClaimedEdges] {
        final def apply(a: ClaimedEdges): Json = (Json.obj(
          ("us", a.us.asJson),
          ("numPlayers", a.numPlayers.asJson),
          ("mines", a.mines.asJson),
          ("futures", a.futures.asJson),
          ("targetSites", a.targetSites.asJson),
          ("graph", a.graph.asJson),
          ("our_graph", a.our_graph.asJson),
          ("game_graph", a.game_graph.asJson)
        ))
    }

    implicit val decodeClaimedEdges: Decoder[ClaimedEdges] = new Decoder[ClaimedEdges] {
        final def apply(c: HCursor): Decoder.Result[ClaimedEdges] = for {
          us <- c.downField("us").as[Int]
          numPlayers <- c.downField("numPlayers").as[Int]
          mines <- c.downField("mines").as[List[SiteId]]
          futures <- c.downField("futures").as[List[T_future]]
          targetSites <- c.downField("targetSites").as[List[SiteId]]
          graph <- c.downField("graph").as[SiteGraph]
          our_graph <- c.downField("our_graph").as[SiteGraph]
          game_graph <- c.downField("game_graph").as[SiteGraph]
        } yield {
          new ClaimedEdges(us, numPlayers, idsToSites(mines), futures, idsToSites(targetSites), graph, our_graph, game_graph)
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
  var targetSites: List[Site],
  var graph: SiteGraph, // unclaimed edges (routable stuff)
  var our_graph: SiteGraph, // our claimed edges
  var game_graph: SiteGraph // the whole game. graph objects are immutable so ok to pass reference
) extends State[ClaimedEdges] {
  
  def this(us: Int, numPlayers: Int, mines: List[Site], futures: List[T_future], targetSites: List[Site], graph: SiteGraph) {
    this(us, numPlayers, mines, futures, targetSites, graph, Graph(), graph)
  }

  override def update(claimed: List[(PunterId, River)]) : ClaimedEdges = {
    // remove an edge *and nodes*, if they are disconnected
    // http://www.scala-graph.org/guides/core-operations.html
    // graph -! source~target
    

    for ((punter, river) <- claimed) {
      val edge = river.edge
      graph = graph -! edge
      val (src, tgt) = (Site(river.source), Site(river.target))
      if (lambda.traceur.helpers.Helpers.enableLoggingForPunter == us) {
        lambda.traceur.helpers.Helpers.gameLogMoves += ("{\"punter\": " + punter + ",\"source\": " + src + ",\"target\": " + tgt + "}")
      }
      if (punter == us) {
        our_graph = our_graph + edge
      }
    }
    return this
  }

  /* returns a function that will calculate the shortest distance between nodes.
   * use in conjunction with Site.distanceTo for caching wins */
  def distanceFn(from: Site, to: Site): () => Int = {
     val g = game_graph
     return () => {
       g.find(from).get.shortestPathTo(g.find(to).get).map(_.edges.size).getOrElse(0)
     }: Int
  }
}

class MagicBrain extends Brains[ClaimedEdges] {

  override def init(me: PunterId, numPlayers: Int, map: R_map, futuresEnabled: Boolean) : ClaimedEdges = {
    val graph = mapToGraph(map)
    val mineSites = idsToSites(map.mines)
    // Try just going for mine with most edges to start
    // instead of most close to other mines as getStrategy used to
    // debug("mineSites: "+mineSites)

    // Without Futures
    // val targetSites = mineSites
    // With Futures
    val (mines, futures, targetSites) = getStrategy(mineSites, graph, numPlayers, true)
    // debug("futures: "+futures)

    new ClaimedEdges(me, numPlayers, mineSites, List(), targetSites, graph)
  }

  def getStrategy(mineSites: List[Site], graph: Graph[Site, UnDiEdge], numPlayers: Int, futuresEnabled: Boolean) : Tuple3[List[Site], List[T_future], List[Site]] = {

    val shouldUseFutures = futuresEnabled && (graph.nodes.size / numPlayers > 30)

    var mines = if(shouldUseFutures) getMinesLongest(mineSites, graph) else getMinesShortest(mineSites, graph)
    
    debug("getStrategy: mines = " + mines.mkString(" "))
    var futures = List[T_future]()
    var targetSites = List[Site]()
    if(!shouldUseFutures){
      return (mines, futures, mines)
    }
    targetSites = targetSites :+ mines(0)
    for(i <- List.range(0, mines.size-1) if i % 2 == 0) {
      val fs = shortestPath(mines(i), mines(i+1), graph) match {
        case None => List[T_future]()
        case Some(path) => List(T_future(mines(i+1), path.edges.toList(1)._2.value), T_future(mines(i), path.edges.toList(path.edges.size-2)._2.value))
      }
      // debug("fs: "+fs)
      // limit futures
      // debug("i,max: "+i+" "+(1.0/30.0 * graph.nodes.size * mines.size / numPlayers).toInt+" "+graph.nodes.size+" "+mines.size+" "+numPlayers)
      if(i <= 8){
        futures = futures ::: fs
        // debug("futures: "+futures)
        targetSites = targetSites ++ fs.map(f=>Site(f.target))
      }
      targetSites = targetSites :+ mines(i+1)
    }

    futures = futures.filter(f=>mines.contains(Site(f.source)) && !mines.contains(Site(f.target)))
    debug("futures: "+futures)
    targetSites = targetSites.filter(t=>mines.contains(t) || futures.map(f=>f.target).contains(t)).distinct

    (mines, futures, targetSites)
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

  def getMinesLongest(mines: List[Site], graph: SiteGraph) : List[Site] = {
    debug("getMinesLongerst: mines = " + mines.mkString(" "))
    if(mines.size<=2){
      return mines
    }

    var ds = getAllDistances(mines, graph).reverse
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

  // This get's the fastest path around all the mines.
  // It won't necessarily grab a lot of mines early on though
  def getMinesShortest(mines: List[Site], graph: SiteGraph) : List[Site] = {
    debug("getMinesLongerst: mines = " + mines.mkString(" "))
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
    // state.futures.filter(future => state.our_graph.find(Site(future.target)) == None && graph.find(Site(future.target)) != None).map(f=>Site(f.target)) ::: state.mines.filter(mine => state.our_graph.find(mine) == None && graph.find(mine) != None)
    var ret = state.targetSites.filter(site => state.our_graph.find(site) == None && graph.find(site) != None)
    // debug("getTargetSites: returning disconnected mines " + ret.mkString(" "))

    // sort by sites with most available edges in graph
    return ret.sortWith(graph.get(_).edges.size > graph.get(_).edges.size)
  }

  // xx for now, assume we have one connected graph
  // get all sites that we have claimed
  // get all mines we have claimed
  // calculate shortest path from all claimed mines to all claimed sites, *using full game graph*
  // score is sum(d^2)

  // https://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
  def profile[R](code: => R, t: Long = System.nanoTime) = (code, System.nanoTime - t)

  def siteScore(site: Site, mines: List[Site], game_graph: Graph[Site, UnDiEdge]) : Int = {
    var score: Int = 0;
    for (mine <- mines) {
      val gg_mine_node = game_graph.get(mine)

      // for (site <- state.our_graph.nodes.toList) {
      // val site_s: Site = site.value

      // site must be reachable from mine on our_graph
      // if (our_graph.find(site_s).get.isPredecessorOf(our_graph.find(mine).get)) {
      val length = gg_mine_node.distanceTo(site, () => {
        gg_mine_node.shortestPathTo(game_graph.get(site)).map(x => x.edges.size).getOrElse(0)
      } : Int)

      score += length * length
      // debug("ourscore: mine " + mine + " to site " + site + " has shortest path " + length + ", cumulative score " + score)
      // }
      
    }
    score

  }

  def ourScore(state: ClaimedEdges) : Int = {
    var score: Int = 0;
    var score_futures: Int = 0;
    var mm: List[Site] = state.mines;
    val (result, time) = profile {
      val our_graph: SiteGraph = state.our_graph
      val game_graph: SiteGraph = state.game_graph
      for (site <- state.our_graph.nodes.toList) {
        score += siteScore(site.value, connectedMines(state), state.game_graph)
      }

      // futures
      if (state.futures.length > 0) {
        debug("ourscore: " + state.futures.length + " futures")
        for (future <- state.futures) {
          val future_source_s: Site = Site(future.source)
          val future_target_s: Site = Site(future.target)
          val gg_future_mine_node = game_graph.find(future_source_s).get
          val gg_future_site_node = game_graph.find(future_target_s).get

          val length = gg_future_mine_node.distanceTo(future_target_s, () => {
            gg_future_mine_node.shortestPathTo(gg_future_site_node).map(x => x.edges.size).getOrElse(0)
          } : Int)

          our_graph.find(future_source_s) match {
            case None => {
              // we didn't capture this mine
              debug("ourscore: futures: didn't capture mine " + future.source + ": " + (-length * length * length))
              score_futures -= length * length * length
            }
            case Some(future_source) => {
              val future_length = future_source.distanceTo(future_target_s, () => {
                gg_future_mine_node.shortestPathTo(gg_future_site_node).map(x => x.edges.size).getOrElse(0)
              } : Int)

              val s = (if (future_length == 0) -(length*length*length) else (length*length*length))
              debug("ourscore: futures: score from mine " + future.source + " to site " + future.target + ": " + s)
              score_futures += s
            }
          }

        }
        //debug("ourscore: our graph: " + state.our_graph.mkString(" "))
        //debug("ourscore: game graph: " + state.game_graph.mkString(" "))
        debug("ourscore: score " + score + " + (" + score_futures + ") = " + (score+score_futures))
      }
    }
    // log slow stuff > 10ms only
    if (time > 10 * 1000 * 1000) debug(s"🐌🐌 ourScore took ${time / (1000 * 1000)}ms 🐌🐌")
    return (score+score_futures)
  }

  def getPathToCurrentTarget(state : ClaimedEdges) : Option[PathType] = {
    val graph = state.graph
    val our_graph = state.our_graph
    var tgtSites = getTargetSites(state)
    var start: Option[Site] = None

    /* special case for first move; start at first target and try to path to others */
    if (our_graph.isEmpty && tgtSites.nonEmpty) {
      start = Some(tgtSites.head)
      tgtSites = tgtSites.tail
    }

    for (target <- tgtSites) {
      // find shortest path to any node we've connected
      for (n <- graph.get(target).withKind(BreadthFirst)) {
        if (our_graph.contains(n.value) || start.contains(n.value)) {
          return n.shortestPathTo(graph.get(target))
        }
      }
    }

    if (start.nonEmpty) {
      return graph.find(start.get).flatMap(_.pathUntil(x => true)) // couldn't path to any targets; return any old neighbour
    }
    return None // we've connected all sites of interest, or they've been blocked
  }

  def tryConnectTargets(state: ClaimedEdges) : Option[River] = {
      getPathToCurrentTarget(state) match {
        case Some(path) => {
          debug("remaining targets: " + getTargetSites(state) + " path: " + path)
          Some(path.edges.head)
        }
        case _ => None
      }
  }

  def tryGreedyNeighbours(state: ClaimedEdges) : Option[River] = {
      val connectedMines = state.mines.filter(state.our_graph.contains(_))
      if (connectedMines.isEmpty)
        return None
      def pointsForSite(site: Site): Int = {
        var points: List[Int] = connectedMines.map(mine => { val d = mine.distanceTo(site, state.distanceFn(mine, site)); d*d })
        points.reduceLeft(_ + _)
      }
      val (graph, our_graph) = (state.graph, state.our_graph)
      our_graph.nodes
        .map(graph.find(_)).flatten    // find our connected nodes in the main graph
        .map(node => node.diSuccessors.filter(!our_graph.contains(_)).map((node, _))) // get not-yet connected neighbours
        .flatten.map(x => (x._1, x._2, pointsForSite(x._2)))
        .reduceOption((best, x) => if (x._3 > best._3) x else best) match {
          case Some((node, next, points)) => Some(River(node, next))
          case None => None
      }
  }

  def connectedMines(state: ClaimedEdges): List[Site] = {
    state.mines.filter(!state.our_graph.find(_).isEmpty)
  }

  def tryFindFurthestTarget(state: ClaimedEdges) : Option[Site] = {
    var bestScore = 0
    var site : Option[Site] = None
    val mines = connectedMines(state)
    for(p <- state.graph.nodes.toList){
      val score = siteScore(p.value, mines, state.game_graph)
      if(score > bestScore){
        bestScore = score
        site = Some(p.value)
      }
    }
    site
  }

  def tryFindLongestRoute(state: ClaimedEdges) : Option[River] = {
    val longestSite = tryFindFurthestTarget(state)
    longestSite match {
      case Some(site) => {
        state.targetSites = List(site)
        tryConnectTargets(state)
      }
      case None => None
    }
  }


  override def nextMove(state: ClaimedEdges) : River = {
    val (claim, time) = profile[River] {
      val strats: List[ClaimedEdges => Option[River]] = List(
        tryConnectTargets,
        tryFindLongestRoute,
        tryGreedyNeighbours
      )

      strats.iterator.map(_(state)).collectFirst { case Some(river) => river } match {
        case Some(river) => river
        case None => {
          debug("failed to find any good moves 😭😭😭")
          state.graph.edges.head
        }
      }
    }
    // log slow stuff > 100ms only
    if (time > 100 * 1000 * 1000) debug(s"🐌🐌 nextMove took ${time / (1000 * 1000)}ms 🐌🐌")
    return claim
  }
}
