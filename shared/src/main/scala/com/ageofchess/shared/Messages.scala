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

  case class InitializeBoard(board: Board, pieces: mutable.Map[Location, RenderablePiece]) extends GameMessage
  object InitializeBoard {
    implicit val rw: ReadWriter[InitializeBoard] = macroRW
  }

  case class AssignPlayers(player: Player, opponent: Player) extends GameMessage
  object AssignPlayers {
    implicit val rw: ReadWriter[AssignPlayers] = macroRW
  }

  sealed trait ClientMessage
  object ClientMessage {
    implicit val rw: ReadWriter[ClientMessage] = macroRW
  }

  case class ConnectPlayer(placeholder: String) extends ClientMessage // add player IDs later
  object ConnectPlayer {
    implicit val rw: ReadWriter[ConnectPlayer] = macroRW
  }
}
