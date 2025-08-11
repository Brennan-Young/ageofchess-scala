package com.ageofchess.shared

import com.ageofchess.shared.piece._
import collection.mutable
import com.ageofchess.shared.board._
import com.ageofchess.shared.user.UserId
import com.ageofchess.shared.user._
import scala.util.Random
import java.time.Instant
import scala.concurrent.duration._

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

  def constructActiveGame(
    gameId: String,
    player1: UserId,
    player2: UserId,
    spectators: List[UserId]
  ): ActiveGame = {

    val coin = Random.nextInt(2)

    val (whitePlayerId, blackPlayerId) = if (coin == 1) (player1, player2) else (player2, player1)
    val white = Player(whitePlayerId, White)
    val black = Player(blackPlayerId, Black)

    val board = BoardGenerator.generateBoard(20)
    val pieces = defaultPieces
    val gold = Map(white -> 100, black -> 100)
    val treasures = Set(Location(0, 1))

    val gameState = GameState(
      gameId,
      white,
      black,
      board,
      pieces,
      gold,
      treasures,
      white
    )

    ActiveGame(
      gameState,
      Map(white -> PlayerClock(1.minute, Instant.now().toEpochMilli), black -> PlayerClock(1.minute, Instant.now().toEpochMilli)),
      List(white, black),
      spectators.map(Spectator(_))
    )

  }
}
