package com.ageofchess.shared

import upickle.default.{ReadWriter, macroRW}
import com.ageofchess.shared.board.Board
import com.ageofchess.shared.user.Player
import com.ageofchess.shared.game.PlayerClock
import com.ageofchess.shared.piece.{Location, Piece}
import com.ageofchess.shared.game._
import com.ageofchess.shared.user.UserId

object Messages {

  sealed trait GameMessage
  object GameMessage {
    implicit val rw: ReadWriter[GameMessage] = macroRW
  }

  case class InitializeBoard(
    board: Board,
    pieces: Map[Location, Piece],
    treasures: Set[Location],
    gold: Map[Player, Int]
  ) extends GameMessage
  object InitializeBoard {
    implicit val rw: ReadWriter[InitializeBoard] = macroRW
  }

  // case class AssignPlayers(player: Player, opponent: Player) extends GameMessage
  // object AssignPlayers {
  //   implicit val rw: ReadWriter[AssignPlayers] = macroRW
  // }

  // TODO: Use this with a playerId-based GamePage
  case class AssignPlayers(white: Player, black: Player) extends GameMessage
  object AssignPlayers {
    implicit val rw: ReadWriter[AssignPlayers] = macroRW
  }

  case class UpdatePieces(nextActivePlayer: Player, pieces: Map[Location, Piece]) extends GameMessage
  object UpdatePieces {
    implicit val rw: ReadWriter[UpdatePieces] = macroRW
  }

  case class UpdateBoardState(nextActivePlayer: Player, playerAction: PlayerAction, pieces: Map[Location, Piece], gold: Map[Player, Int], treasures: Set[Location]) extends GameMessage
  object UpdateBoardState {
    implicit val rw: ReadWriter[UpdateBoardState] = macroRW
  }

  case class UpdatePlayerClocks(clocks: Map[Player, PlayerClock]) extends GameMessage
  object UpdatePlayerClocks {
    implicit val rw: ReadWriter[UpdatePlayerClocks] = macroRW
  }

  case class UpdateLobbies(lobbies: List[Lobby]) extends GameMessage
  object UpdateLobbies {
    implicit val rw: ReadWriter[UpdateLobbies] = macroRW
  }

  case class ResolveGame(result: GameResult) extends GameMessage
  object ResolveGame {
    implicit val rw: ReadWriter[ResolveGame] = macroRW
  }

  case class CreateGameRequest(initialClockSeconds: Int, boardSize: Int)
  object CreateGameRequest {
    implicit val rw: ReadWriter[CreateGameRequest] = macroRW
  }

  case class CreateGameResponse(gameId: String)
  object CreateGameResponse {
    implicit val rw: ReadWriter[CreateGameResponse] = macroRW
  }

  sealed trait ClientMessage
  object ClientMessage {
    implicit val rw: ReadWriter[ClientMessage] = macroRW
  }

  case class ConnectPlayer(placeholder: String) extends ClientMessage // add player IDs later
  object ConnectPlayer {
    implicit val rw: ReadWriter[ConnectPlayer] = macroRW
  }

  case class AwaitingBoard(userId: UserId) extends ClientMessage
  object AwaitingBoard {
    implicit val rw: ReadWriter[AwaitingBoard] = macroRW
  }

  case class FetchLobbies() extends ClientMessage
  object FetchLobbies {
    implicit val rw: ReadWriter[FetchLobbies] = macroRW
  }

  sealed trait PlayerActionMessage extends ClientMessage {
    def player: Player
    def toPlayerAction: PlayerAction
  }
  object PlayerActionMessage {
    implicit val rw: ReadWriter[PlayerActionMessage] = macroRW
  }

  case class MovePiece(player: Player, piece: Piece, from: Location, to: Location) extends PlayerActionMessage { // TODO: playerIds, piece movement aren't strictly needed yet but perhaps have benefits?
    override def toPlayerAction: PlayerAction = {
      PieceMove(piece, from, to)
    }
  }

  object MovePiece {
    implicit val rw: ReadWriter[MovePiece] = macroRW
  }

  case class PlacePiece(player: Player, piece: Piece, location: Location) extends PlayerActionMessage {
    override def toPlayerAction: PlayerAction = {
      PiecePlacement(piece, location)
    }
  }

  object PlacePiece {
    implicit val rw: ReadWriter[PlacePiece] = macroRW
  }
}
