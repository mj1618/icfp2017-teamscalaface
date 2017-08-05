package lambda
package traceur
import scalax.collection.Graph
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scala.math.pow
import scala.util.Random

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
}
