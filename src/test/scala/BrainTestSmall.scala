import org.scalatest.{FlatSpec,Matchers}

import lambda.traceur._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.onlinemsg.Msg._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class BrainSmallSpec extends FlatSpec with Matchers {
	val me : PunterId = 1
	val sample = scala.io.Source.fromFile("samples/circle.json").mkString

	it should "generate mine ordering" in {
		var brain = new MagicBrain()
		var state = brain.init(me, 2, decode[R_map](sample).right.get)
		println(brain.highestValueMines(state))
	}
}
