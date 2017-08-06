package lambda
package traceur
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scala.collection.Map
import scala.collection.mutable.HashMap
import scala.math.pow
import scala.util.Random

import io.circe._,  io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Types {
	type PunterId = Int
	type SiteId = Int

	abstract sealed case class River(source: SiteId, target: SiteId) {
		def edge(): UnDiEdge[Site] = { Site(source) ~ Site(target) }
	}

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

		def apply(source: Site, target: Site): River = apply(source.id, target.id)
	}
	type SiteGraph = Graph[Site, UnDiEdge]
	type PathType = SiteGraph#Path
	sealed abstract class Move(punter: PunterId)
	case class Claim(punter: PunterId, river: River) extends Move(punter)
	case class Pass(punter: PunterId) extends Move(punter)

	type Score = (PunterId, Int)

    implicit val encodeGraph: Encoder[SiteGraph] = new Encoder[SiteGraph] {
         final def apply(a: SiteGraph): Json = (Json.obj( ("nodes", a.nodes.map((x: SiteGraph#NodeT) => x.value).asJson), ("edges", (for {
            edges <- a.edges
         } yield {
            (edges._1.value.id, edges._2.value.id)
         }).asJson) ))
     }

     implicit val decodeGraph: Decoder[SiteGraph] = new Decoder[SiteGraph] {
         final def apply(c: HCursor): Decoder.Result[SiteGraph] = Right(Graph.from(
            (c.downField("nodes").focus.head.asArray.get.map((x => x.as[Site].right.get))),
            (c.downField("edges").focus.head.asArray.get.map((x => Site(x.asArray.get(0).as[SiteId].right.get) ~ Site(x.asArray.get(1).as[SiteId].right.get))))
         ))
     }

	class Site private(val id: SiteId) {
		private val distanceCache : HashMap[SiteId, Int] = HashMap.empty[SiteId, Int]

		/* returns Some(distance) to another site, if known (otherwise returns None) */
		def distanceTo(other: SiteId) : Option[Int] = distanceCache.synchronized {
			distanceCache.get(other)
		}

		/* returns distance to another site, calculating it via distFn if necessary */
		def distanceTo(other: SiteId, distFn: () ⇒ Int) : Int = distanceCache.synchronized {
			distanceCache.get(other) match {
				case Some(d) => d
				case None => {
					val d = distFn()
					distanceCache.put(other, d)
					d
				}
			}
		}

		override def toString() : String = {
			id.toString
		}

		override def hashCode() : Int = {
			id.hashCode
		}
	}

	object Site {
		private val instances = new java.util.HashMap[SiteId, Site]

		def apply(id: SiteId) : Site = instances.synchronized {
			var s = instances.get(id)
			if (s == null) {
				s = new Site(id)
				instances.put(id, s)
			}
			return s
		}

		implicit def site2id(site: Site) : SiteId = { site.id }

		/* a Site is encoded as a raw number if its distanceCache is empty, otherwise {id:X, d:[[id2,dist2],...]} */
		implicit val encodeSite: Encoder[Site] = new Encoder[Site] {
			final def apply(site: Site): Json = {
				if (site.distanceCache.isEmpty)
					site.id.asJson
				else
					Json.obj(("id", site.id.asJson), ("d", site.distanceCache.toList.asJson))
			}
		}

		implicit val decodeSite: Decoder[Site] = new Decoder[Site] {
			final def apply(c: HCursor): Decoder.Result[Site] = {
				c.focus match {
					case Some(json) if json.isNumber => Right(Site(c.as[SiteId].right.get))
					case Some(json) if json.isObject => {
						val obj = json.asObject.get
						val s = Site(obj("id").get.as[SiteId].right.get)
						for ((target, distance) <- obj("d").get.as[Vector[(SiteId, Int)]].right.get) {
							s.distanceTo(target, () => {distance}: Int)
						}
						Right(s)
					}
					case None => Left(DecodingFailure("invalid Site", c.history))
				}
			}
		}
	}
}

