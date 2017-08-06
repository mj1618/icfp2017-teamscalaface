import org.scalatest.{FlatSpec,Matchers}

import lambda.traceur._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.onlinemsg.Msg._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class BrainSmallSpec extends FlatSpec with Matchers {
	val me : PunterId = 1
	val sample = scala.io.Source.fromFile("samples/nara-sparse.json").mkString

	it should "generate mine ordering longest" in {
		var brain = new MagicBrain()
		var state = brain.init(me, 2, decode[R_map](sample).right.get, false)
		val res = brain.getMinesLongest(state.mines,state.graph)
		println(res)
		res should be (idsToSites(List(760, 1552, 0, 1510, 243, 455, 561, 262, 176, 1105, 1019, 754)))
	}


	it should "generate mine ordering fastest" in {
		var brain = new MagicBrain()
		var state = brain.init(me, 2, decode[R_map](sample).right.get, false)
		val res = brain.getMinesFastest(state.mines,state.graph)
		println(res)
		res should be (idsToSites(List(760, 1552, 243, 455, 262, 176, 1105, 0, 1510, 561, 1019, 754)))
	}


	it should "get strategy should return futures" in {
		var brain = new MagicBrain()
		var state = brain.init(me, 2, decode[R_map](sample).right.get, false)
		println("futures: "+state.futures)
		println("targetSites: "+state.targetSites)
		// res should be (idsToSites(List(760, 1552, 0, 1510, 243, 455, 561, 262, 176, 1105, 1019, 754)))
	}

}
