package lambda
package traceur

object Types {
	type PunterId = Int
	type SiteId = Int
	case class River(source: SiteId, target: SiteId)

	sealed abstract class Move(punter: PunterId)
	case class Claim(punter: PunterId, river: River) extends Move(punter)
	case class Pass(punter: PunterId) extends Move(punter)

	type Score = (PunterId, Int)
}
