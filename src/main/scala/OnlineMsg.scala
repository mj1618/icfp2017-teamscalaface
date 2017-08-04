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

  // {"punter":0,"punters":2,"map":{"sites":[{"id":4,"x":2.0,"y":-2.0},{"id":1,"x":1.0,"y":0.0},{"id":3,"x":2.0,"y":-1.0},{"id":6,"x":0.0,"y":-2.0},{"id":5,"x":1.0,"y":-2.0},{"id":0,"x":0.0,"y":0.0},{"id":7,"x":0.0,"y":-1.0},{"id":2,"x":2.0,"y":0.0}],"rivers":[{"source":3,"target":4},{"source":0,"target":1},{"source":2,"target":3},{"source":1,"target":3},{"source":5,"target":6},{"source":4,"target":5},{"source":3,"target":5},{"source":6,"target":7},{"source":5,"target":7},{"source":1,"target":7},{"source":0,"target":7},{"source":1,"target":2}],"mines":[1,5]}
  case class R_site(id: SiteId, x: Float, y: Float)
  case class R_river(source: SiteId, target: SiteId)
  case class R_map(sites: List[R_site], rivers: List[R_river], mines: List[Int])
	case class R_setup(punter: PunterId, punters: Int, map: R_map) extends Msg
	case class T_setup(id: PunterId) extends Msg

	case class R_gameplay(moves: List[Move]) extends Msg // one move per punter
	case class T_gameplay(move: Move) extends Msg

	case class R_scoring(moves: List[Move], scores: List[Score]) extends Msg
}
