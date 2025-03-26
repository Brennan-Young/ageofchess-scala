package com.ageofchess.shared

import upickle.default.{ReadWriter, readwriter, macroRW}
import com.ageofchess.shared.board.Board
import com.ageofchess.shared.game.Player
import com.ageofchess.shared.piece.{Location, RenderablePiece}
import collection.mutable

object Messages {

  sealed trait GameMessage
  object GameMessage {
    implicit val rw: ReadWriter[GameMessage] = macroRW
  }

  case class InitializeBoard(board: Board, pieces: Map[Location, RenderablePiece]) extends GameMessage
  object InitializeBoard {
    implicit val rw: ReadWriter[InitializeBoard] = macroRW
  }

  case class AssignPlayers(player: Player, opponent: Player) extends GameMessage
  object AssignPlayers {
    implicit val rw: ReadWriter[AssignPlayers] = macroRW
  }

  case class UpdatePieces(pieces: Map[Location, RenderablePiece]) extends GameMessage
  object UpdatePieces {
    implicit val rw: ReadWriter[UpdatePieces] = macroRW
  }

  sealed trait ClientMessage
  object ClientMessage {
    implicit val rw: ReadWriter[ClientMessage] = macroRW
    // implicit val rw: ReadWriter[ClientMessage] = ReadWriter.merge(
    //   macroRW[ConnectPlayer],
    //   macroRW[MovePiece]
    // )
  }

  case class ConnectPlayer(placeholder: String) extends ClientMessage // add player IDs later
  object ConnectPlayer {
    implicit val rw: ReadWriter[ConnectPlayer] = macroRW
  }

  case class MovePiece(from: Location, to: Location) extends ClientMessage // TODO: playerIds, piece movement aren't strictly needed yet but perhaps have benefits?
  object MovePiece {
    implicit val rw: ReadWriter[MovePiece] = macroRW
  }
}
