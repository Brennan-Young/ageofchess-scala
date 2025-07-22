package com.ageofchess.shared

import upickle.default.{ReadWriter, readwriter, macroRW}
import com.ageofchess.shared.board.Board
import com.ageofchess.shared.game.Player
import com.ageofchess.shared.piece.{Location, Piece}
import collection.mutable
import scala.concurrent.Await

object Messages {

  sealed trait GameMessage
  object GameMessage {
    implicit val rw: ReadWriter[GameMessage] = macroRW
  }

  case class InitializeBoard(board: Board, pieces: Map[Location, Piece]) extends GameMessage
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

  case class MovePiece(player: Player, from: Location, to: Location) extends ClientMessage // TODO: playerIds, piece movement aren't strictly needed yet but perhaps have benefits?
  object MovePiece {
    implicit val rw: ReadWriter[MovePiece] = macroRW
  }

  case class PlacePiece(player: Player, piece: Piece, location: Location) extends ClientMessage
  object PlacePiece {
    implicit val rw: ReadWriter[PlacePiece] = macroRW
  }
}
