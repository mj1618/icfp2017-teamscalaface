package lambda
package traceur
package helpers

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.edge.WUnDiEdge
import scalax.collection.GraphPredef._

import java.io.PrintWriter
import sys.process._
import java.net.{ServerSocket, Socket}
import resource._
import scala.util.control.Breaks._

import lambda.traceur.MagicBrain, lambda.traceur.ClaimedEdges
import lambda.traceur.onlinemsg.Msg
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import scala.io.Source.fromFile
import scala.util.Random

object Helpers {
	def mapToGraph(mp: R_map): Graph[SiteId, UnDiEdge] = Graph.from(
		for (site <- mp.sites) yield site.id, 
		for (river <- mp.rivers) yield river.source ~ river.target
	)

	def randomFromList[T](ls : List[T]) : Option[T]  = {
		if(ls.size>0)
			Some(ls(Random.nextInt(ls.size)))
		else
			None
	}

	def shortestPath(a: SiteId, b: SiteId, graph: Graph[SiteId, UnDiEdge]) : Int = {
		if(graph.find(a).isEmpty || graph.find(b).isEmpty || graph.find(a).get.shortestPathTo(graph.find(b).get).isEmpty)
			0
		else
			graph.find(a).get.shortestPathTo(graph.find(b).get).get.nodes.size
	}

	def loadMap(filename: String): R_map = decode[R_map](fromFile(filename).mkString).right.get

	def mockBrain(filename: String): (MagicBrain, ClaimedEdges) = {
		var brain = new MagicBrain()
		var state = brain.init(1, 5, loadMap(filename))
		(brain, state)
	}

	def debug(s: Any) : Unit = {
		Console.err.println(s)
	}
}
