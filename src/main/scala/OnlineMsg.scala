package lambda
package traceur
package onlinemsg

import lambda.traceur.Types._

sealed abstract class Msg

/* Really basic message representations. Feel free to change types around as we settle on data structures */
object Msg {
	/* prefix is T for transmitted messages, R for recieved */
	case class T_handshake(me: String) extends Msg
	case class R_handshake(you: String) extends Msg

	case class R_setup(id: PunterId, numPunters: Int, sites: List[SiteId], rivers: List[River], mines: List[SiteId]) extends Msg
	case class T_setup(id: PunterId) extends Msg

	case class R_gameplay(moves: List[Move]) extends Msg // one move per punter
	case class T_gameplay(move: Move) extends Msg

	case class R_scoring(moves: List[Move], scores: List[Score]) extends Msg
}
