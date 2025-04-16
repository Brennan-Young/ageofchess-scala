package com.ageofchess.client.gamestate

import com.raquo.laminar.api.L._
import com.ageofchess.client.api.Sockets
import com.ageofchess.client.api.Sockets.GameSocket
import com.ageofchess.shared.game._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._
import com.ageofchess.shared.Messages._
import upickle.default._

class ClientGame(
  val gameId: String,
  val player: Player,
  val opponent: Player,
  val startingPlayer: Player,
  val connection: GameSocket
) {
  val piecesVar: Var[Map[Location, Piece]] = Var(Map())
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val selectedPiece: Var[Option[(Location, Piece)]] = Var(None)
  
  connection.socket.onmessage = event => {
    val message: GameMessage = read[GameMessage](event.data.toString)
    println(s"Received game message: $message")
    message match {
      case InitializeBoard(board, pieces) => {
        println("Initializing board")
        boardVar.set(Some(board.squares))
        piecesVar.set(pieces)
      }
      case _ =>
    }
  }

  val moveTurnBus = new EventBus[Unit]
  val playerToMoveSignal: Signal[Player] = moveTurnBus.events.scanLeft(startingPlayer) { case (p, _) =>
    println(p)
    if (p == player) opponent else player 
  }

  val isPlayerTurnSignal: Signal[Boolean] = playerToMoveSignal.map { p =>
    if (p == player) true else false
  }

  val validMovesSignal = Signal.combine(boardVar.signal, piecesVar.signal, selectedPiece.signal).map {
    case (Some(board), pieces, Some((location, piece))) => {
      validMoves(BoardWithPieces(board, pieces), location, piece)
    }
    case _ => Set.empty[Location]
  }

  val validCapturesSignal = Signal.combine(boardVar.signal, piecesVar.signal, selectedPiece.signal).map {
    case (Some(board), pieces, Some((location, piece))) => {
      validCaptures(BoardWithPieces(board, pieces), location, piece)
    }
    case _ => Set.empty[Location]
  }
}

class PendingClientGame(
  val gameId: String,
  val connection: GameSocket
) {
  val playerVar: Var[Option[Player]] = Var(None)
  val opponentVar: Var[Option[Player]] = Var(None)
  val startingPlayerVar: Var[Option[Player]] = Var(None)

  val initializedPlayersSignal = Signal.combine(
    playerVar.signal,
    opponentVar.signal,
    startingPlayerVar.signal
  ).map {
    case (Some(player), Some(opponent), Some(startingPlayer)) => {
      Some((player, opponent, startingPlayer))
    }
    case _ => {
      None
    }
  }

  connection.socket.onopen = _ => {
    connection.socket.send(write(ConnectPlayer("placeholder")))
  }

  connection.socket.onmessage = event => {
    val message: GameMessage = read[GameMessage](event.data.toString)
    println(s"Received pending game message: $message")
    message match {
      case AssignPlayers(player, opponent) => {
        println("Assigning players")
        playerVar.set(Some(player))
        opponentVar.set(Some(opponent))
        val startingPlayer = if (player.color == White) player else opponent
        startingPlayerVar.set(Some(startingPlayer))
      }
      case _ =>
    }
  }
}
