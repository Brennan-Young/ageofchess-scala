package com.ageofchess.shared

import com.ageofchess.shared.piece._
import upickle.default._
import collection.mutable
import com.ageofchess.shared.board._

package object game {
  sealed trait GameUser {
    def id: String
  }

  object GameUser {
    implicit val rw: ReadWriter[GameUser] = macroRW
  }

  case class Player(id: String, color: Color) extends GameUser

  object Player {
    implicit val rw: ReadWriter[Player] = macroRW
  }

  // TODO: A spectator is not an element of "pure" chess the way a player is - move this
  case class Spectator(id: String) extends GameUser

  object Spectator {
    implicit val rw: ReadWriter[Spectator] = macroRW
  }

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
}
