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

  case class GameSettings(initialClock: FiniteDuration, boardSize: Int)
  object GameSettings {
    val MinBoardSize = 6
    val MaxBoardSize = 20
    def fromSeconds(seconds: Int, boardSize: Int = 10): GameSettings = {
      val clamped = math.max(MinBoardSize, math.min(MaxBoardSize, boardSize))
      GameSettings(seconds.seconds, clamped)
    }
    val default: GameSettings = GameSettings(1.minute, 10)
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


  case class Pending(
    gameId: String,
    players: List[UserId],
    spectators: List[UserId],
    gameSettings: GameSettings = GameSettings.default
  )

  def constructActiveGame(
    gameId: String,
    player1: UserId,
    player2: UserId,
    spectators: List[UserId],
    gameSettings: GameSettings
  ): ActiveGame = {

    val coin = Random.nextInt(2)

    val (whitePlayerId, blackPlayerId) = if (coin == 1) (player1, player2) else (player2, player1)
    val white = Player(whitePlayerId, White)
    val black = Player(blackPlayerId, Black)

    val board = BoardGenerator.generateBoard(gameSettings.boardSize)
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

    val clock = gameSettings.initialClock
    ActiveGame(
      gameState,
      Map(white -> PlayerClock(clock, Instant.now().toEpochMilli), black -> PlayerClock(clock, Instant.now().toEpochMilli)),
      List(white, black),
      spectators.map(Spectator(_))
    )

  }
}
