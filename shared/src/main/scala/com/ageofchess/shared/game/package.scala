package com.ageofchess.shared

import com.ageofchess.shared.piece._
import collection.mutable
import com.ageofchess.shared.board._
import com.ageofchess.shared.user.UserId
import com.ageofchess.shared.user._

package object game {
  case class Game(
    gameId: String,
    white: Player,
    black: Player,
    board: Board,
    pieces: mutable.Map[Location, Piece],
    treasures: mutable.Set[Location],
    gold: mutable.Map[Player, Int]
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

  case class Pending(
    gameId: String,
    players: List[UserId],
    spectators: List[UserId]
  )
}
