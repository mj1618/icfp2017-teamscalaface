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
import java.io._
import resource._
import scala.util.control.Breaks._

import lambda.traceur.onlinemsg.Msg
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._


object Helpers {

	def gameToGraph(game: R_setup): Graph[SiteId, UnDiEdge] = {
		var g = Graph[SiteId, UnDiEdge]()
	    // print
	    var r = ""
	  	for( r <- game.map.rivers) {
	  		g = g + r.source ~ r.target
	  	}
	  	return g
	}
}