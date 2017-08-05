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
  case class T_setup(ready: PunterId) extends Msg
    
  abstract class BaseGameState(setup: R_setup)
  case class GameState(setup: R_setup) extends BaseGameState(setup)

  // {"move":{"moves":[{"pass":{"punter":0}},{"pass":{"punter":1}}]}}
  // Error decoding JSON: Left(DecodingFailure([A]List[A], List(DownArray, DownField(moves), DownField(move))))
  case class TR_claim_p(punter: PunterId, source: SiteId, target: SiteId) extends Msg
  case class TR_claim(claim: TR_claim_p) extends Msg

  case class TR_punter(punter: PunterId) extends Msg
  case class TR_pass(pass: TR_punter) extends Msg

  case class R_move(moves: List[TR_pass])

  case class R_gameplay(move: R_move) extends Msg // one move per punter
  case class T_gameplay(claim: TR_claim_p) extends Msg

  case class R_scoring(moves: List[Move], scores: List[Score]) extends Msg
    
  case class OT_setup(ready: PunterId, state: GameState) extends Msg
  case class OR_gameplay(move: R_move, state: GameState) extends Msg
  case class OT_gameplay(claim: TR_claim_p, state: GameState) extends Msg

}

