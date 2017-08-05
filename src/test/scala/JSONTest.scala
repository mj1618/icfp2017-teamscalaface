import org.scalatest.{ FlatSpec, Matchers }

import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.lamclient._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class JSONSpec extends FlatSpec with Matchers {
  it should "parse claims" in {
    val data = """[{"claim":{"punter":0,"source":6,"target":7}},{"claim":{"punter":1,"source":7,"target":0}}]"""
    val list = LamClient.parseClaims(0, LamClient.handleCirceResponse(parse(data)).hcursor)
    list(0) == (0, River(6, 7)) should be (true)
    list(1) == (1, River(7, 0)) should be (true)
  }

  it should "parse scores" in {
    val data = """[{"punter":0,"score":6},{"punter":1,"score":6}]"""
    val json = LamClient.handleCirceResponse(parse(data))
    val vec = json.asArray.getOrElse(Vector.empty).map(_.hcursor)
    val elem2tuple: io.circe.HCursor => (PunterId, Int) = cur => {
      (cur.get[PunterId]("punter").getOrElse(-1), cur.get[Int]("score").getOrElse(-1))
    }

    val scores = json.asArray.map(_.map(elem2tuple.compose(_.hcursor))).get
    scores(0) == (0, 6) should be (true)
    scores(1) == (1, 6) should be (true)
  }
}
