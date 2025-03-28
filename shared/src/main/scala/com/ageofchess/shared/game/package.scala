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
    pieces: mutable.Map[Location, Piece],
  ) {
    // TODO: better way to represent this than a var?  Should a "Game" be a sequence of game states rather than a mutable class like this?
    var playerToMove: Player = white

    def changeActivePlayer: Unit = {
      if (playerToMove == white) playerToMove = black
      else playerToMove = white
    }
  }

  case class PendingGame(
    gameId: String,
    player1: String
  )
}
