package com.ageofchess.client.gamestate

import com.raquo.laminar.api.L._
import com.ageofchess.client.api.Sockets
import com.ageofchess.shared.game._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._
import com.ageofchess.shared.Messages._
import upickle.default._

class ClientGameState(val gameId: String) {

  val piecesVar: Var[Map[Location, RenderablePiece]] = Var(Map())
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val playerVar: Var[Option[Player]] = Var(None)
  val opponentVar: Var[Option[Player]] = Var(None)
  val playerToMoveVar: Var[Option[Player]] = Var(None) 
  val selectedPiece: Var[Option[Location]] = Var(None)

  val socket = Sockets.buildSocket(gameId)

  socket.onopen = _ => {
    socket.send("new connection")
  }

  socket.onmessage = event => {
    val message: GameMessage = read[GameMessage](event.data.toString)
    println(s"Received message: $message")
    message match {
      case InitializeBoard(board, pieces) => {
        println("Initializing board")
        boardVar.set(Some(board.squares))
        piecesVar.set(pieces.toMap)
      }
      case AssignPlayers(player, opponent) => {
        println("Assigning players")
        playerVar.set(Some(player))
        opponentVar.set(Some(opponent))
        val startingPlayer = if (player.color == White) player else opponent
        playerToMoveVar.set(Some(startingPlayer))
      }
      case _ =>
    }
  }

  val boardStateSignal = Signal.combine(boardVar.signal, piecesVar.signal).map {
    case (Some(board), pieces) => {
      val zippedBoard = board.zipWithIndex.map { case (row, rIdx) =>
        row.zipWithIndex.map { case (square, cIdx) =>
          val pieceOpt = pieces.get(Location(rIdx, cIdx))
          (square, pieceOpt)
        }  
      }
      Some(zippedBoard)
    }
    case _ => None
  }
}
