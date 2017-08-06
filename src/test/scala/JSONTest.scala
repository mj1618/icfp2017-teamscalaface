import org.scalatest.{ FlatSpec, Matchers }

import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import lambda.traceur.onlinemsg.Msg._
import lambda.traceur.Types._
import lambda.traceur.helpers.Helpers._
import lambda.traceur.lamclient._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class JSONSpec extends FlatSpec with Matchers {
  it should "parse claims" in {
    val data = """[{"claim":{"punter":0,"source":6,"target":7}},{"claim":{"punter":1,"source":7,"target":0}},{"pass":{"punter":2}},
        {"splurge":{"punter":3, "route":[3,4,5,6]}}]"""
    val list = LamClient.parseClaims(0, LamClient.handleCirceResponse(parse(data)).hcursor)
    list(0) == (0, River(6, 7)) should be (true)
    list(1) == (1, River(7, 0)) should be (true)
    list(2) == (3, River(3, 4)) should be (true)
    list(3) == (3, River(4, 5)) should be (true)
    list(4) == (3, River(5, 6)) should be (true)
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


class SiteJSONSpec extends FlatSpec with Matchers {
  var json = """{"nodes":[{"id":1,"d":[[3,1]]},5,2,3],"edges":[[1,3],[1,5],[2,5]]}"""
  var g = decode[SiteGraph](json).right.get

  g == Graph(River(1,3).edge, River(1,5).edge, River(5,2).edge) should be (true)
  Site(1).distanceTo(3) == Some(1) should be (true)

  g.asJson.noSpaces == json should be (true)
}
