package com.ageofchess.client.gamestate

import com.raquo.laminar.api.L._
import com.ageofchess.client.api.Sockets
import com.ageofchess.shared.game._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._
import com.ageofchess.shared.Messages._
import upickle.default._

class ClientGameState(val gameId: String) {
  // TODO: Might be better as a Vector[Vector[(Location, Var[Piece])]], as a grid.  
  // We'd be storing a bunch more in memory, but each square can evolve independently rather 
  // than updating the entire Var every time a piece moves.
  val piecesVar: Var[Map[Location, Piece]] = Var(Map())
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val playerVar: Var[Option[Player]] = Var(None)
  val opponentVar: Var[Option[Player]] = Var(None)
  val playerToMoveVar: Var[Option[Player]] = Var(None) 
  val selectedPiece: Var[Option[Location]] = Var(None)

  // TODO: Not part of the game state, it's just how the client communicates with the server.
  // Should be moved out of this class or the class should be renamed
  val socket = Sockets.buildSocket(gameId)

  socket.onopen = _ => {
    socket.send(write(ConnectPlayer("placeholder")))
  }

  socket.onmessage = event => {
    val message: GameMessage = read[GameMessage](event.data.toString)
    println(s"Received message: $message")
    message match {
      case InitializeBoard(board, pieces) => {
        println("Initializing board")
        boardVar.set(Some(board.squares))
        piecesVar.set(pieces)
      }
      case AssignPlayers(player, opponent) => {
        println("Assigning players")
        playerVar.set(Some(player))
        opponentVar.set(Some(opponent))
        val startingPlayer = if (player.color == White) player else opponent
        // TODO: obviously bad code, fix this
        if (startingPlayer == player) moveTurnBus.emit() else {
          moveTurnBus.emit()
          moveTurnBus.emit()
        }
      }
      case UpdatePieces(pieces) => {
        println("Updating board state")
        piecesVar.set(pieces)
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

  val moveTurnBus = new EventBus[Unit]
  val playerToMoveSignal: Signal[Option[Player]] = moveTurnBus.events.scanLeft(None: Option[Player]) { case (player, _) =>
    if (player == playerVar.now()) opponentVar.now() else playerVar.now()  
  }

  val isPlayerTurnSignal: Signal[Boolean] = Signal.combine(playerVar.signal, playerToMoveSignal).map {
    case (Some(player), Some(playerToMove)) => if (player == playerToMove) true else false
    case _ => false
  }
}
