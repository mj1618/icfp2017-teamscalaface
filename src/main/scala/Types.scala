package lambda
package traceur
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scala.math.pow
import scala.util.Random

import io.circe._,  io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._


object Types {
	type PunterId = Int
	type SiteId = Int

	sealed abstract case class River(source: SiteId, target: SiteId)
	object River {
		/* fun hack to ensure River.source < River.target */
		def apply(source: SiteId, target: SiteId): River = {
			if (source == target) {
				throw new IllegalArgumentException("no circular rivers thx")
			} else if (source < target) {
				new River(source, target){}
			} else {
				new River(target, source){}
			}
		}
	}
	type PathType = Graph[SiteId, UnDiEdge]#Path
	sealed abstract class Move(punter: PunterId)
	case class Claim(punter: PunterId, river: River) extends Move(punter)
	case class Pass(punter: PunterId) extends Move(punter)

	type Score = (PunterId, Int)

    implicit val encodeGraph: Encoder[Graph[SiteId, UnDiEdge]] = new Encoder[Graph[SiteId, UnDiEdge]] {
         final def apply(a: Graph[SiteId, UnDiEdge]): Json = (Json.obj( ("nodes", a.nodes.map((x: Graph[SiteId,UnDiEdge]#NodeT) => x.value).asJson), ("edges", (for {
            edges <- a.edges
         } yield {
            (edges._1.value, edges._2.value)
         }).asJson) ))
     }

     implicit val decodeGraph: Decoder[Graph[SiteId, UnDiEdge]] = new Decoder[Graph[SiteId, UnDiEdge]] {
         final def apply(c: HCursor): Decoder.Result[Graph[SiteId, UnDiEdge]] = Right(Graph.from(
            (c.downField("nodes").focus.head.asArray.get.map((x => x.as[SiteId].right.get))),
            (c.downField("edges").focus.head.asArray.get.map((x => x.asArray.get(0).as[SiteId].right.get ~ x.asArray.get(1).as[SiteId].right.get)))
         ))
     }

}
