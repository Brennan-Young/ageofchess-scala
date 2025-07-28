package com.ageofchess.shared

import upickle.default.{ReadWriter, readwriter, macroRW}
import com.ageofchess.shared.board.Board
import com.ageofchess.shared.game.Player
import com.ageofchess.shared.piece.{Location, Piece}
import com.ageofchess.shared.game._
import collection.mutable
import scala.concurrent.Await

object Messages {

  sealed trait GameMessage
  object GameMessage {
    implicit val rw: ReadWriter[GameMessage] = macroRW
  }

  case class InitializeBoard(board: Board, pieces: Map[Location, Piece], treasures: Set[Location]) extends GameMessage
  object InitializeBoard {
    implicit val rw: ReadWriter[InitializeBoard] = macroRW
  }

  case class AssignPlayers(player: Player, opponent: Player) extends GameMessage
  object AssignPlayers {
    implicit val rw: ReadWriter[AssignPlayers] = macroRW
  }

  case class UpdatePieces(nextActivePlayer: Player, pieces: Map[Location, Piece]) extends GameMessage
  object UpdatePieces {
    implicit val rw: ReadWriter[UpdatePieces] = macroRW
  }

  case class UpdateBoardState(nextActivePlayer: Player, pieces: Map[Location, Piece], gold: Map[Player, Int], treasures: Set[Location]) extends GameMessage
  object UpdateBoardState {
    implicit val rw: ReadWriter[UpdateBoardState] = macroRW
  }

  sealed trait ClientMessage
  object ClientMessage {
    implicit val rw: ReadWriter[ClientMessage] = macroRW
  }

  case class ConnectPlayer(placeholder: String) extends ClientMessage // add player IDs later
  object ConnectPlayer {
    implicit val rw: ReadWriter[ConnectPlayer] = macroRW
  }

  case class AwaitingBoard(playerId: String) extends ClientMessage
  object AwaitingBoard {
    implicit val rw: ReadWriter[AwaitingBoard] = macroRW
  }

  sealed trait PlayerActionMessage extends ClientMessage {
    def player: Player
    def toPlayerAction: PlayerAction
  }
  object PlayerActionMessage {
    implicit val rw: ReadWriter[PlayerActionMessage] = macroRW
  }

  case class MovePiece(player: Player, from: Location, to: Location) extends PlayerActionMessage { // TODO: playerIds, piece movement aren't strictly needed yet but perhaps have benefits?
    override def toPlayerAction: PlayerAction = {
      PieceMove(from, to)
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
