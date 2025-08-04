package com.ageofchess.client.gamestate

import com.raquo.laminar.api.L._
import com.ageofchess.client.api.Sockets.GameSocket
import com.ageofchess.shared.game._
import scala.concurrent.duration._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._

class SpectatorGameView(
  val gameId: String,
  val white: Player,
  val black: Player,
  val connection: GameSocket
) {

  val piecesVar: Var[Map[Location, Piece]] = Var(Map())
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val treasuresVar: Var[Set[Location]] = Var(Set())
  val selectedPiece: Var[Option[(Option[Location], Piece)]] = Var(None)

  val playerGoldVar: Var[Int] = Var(100)
  val opponentGoldVar: Var[Int] = Var(100)

  val playerClockVar = Var(5.minutes)
  val opponentClockVar = Var(5.minutes)

}