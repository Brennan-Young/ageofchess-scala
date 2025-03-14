package com.ageofchess.shared

import com.ageofchess.shared.piece._
import upickle.default._
import collection.mutable
import com.ageofchess.shared.board._

package object game {
  case class Player(id: String, color: Color)

  object Player {
    implicit val rw: ReadWriter[Player] = macroRW
  }

  case class Game(
    gameId: String,
    white: Player,
    black: Player,
    board: Board,
    pieces: mutable.Map[Location, RenderablePiece],
    whiteToMove: Boolean
  )

  case class PendingGame(
    gameId: String,
    player1: String
  )
}
