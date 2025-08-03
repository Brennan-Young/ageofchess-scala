package com.ageofchess.client.gamestate

import com.raquo.laminar.api.L._
import com.ageofchess.client.api.Sockets
import com.ageofchess.client.api.Sockets.GameSocket
import com.ageofchess.shared.game._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._
import com.ageofchess.shared.Messages._
import upickle.default._
import scala.concurrent.duration._
import org.scalajs.dom.MessageEvent

class PlayerGameView(
  val gameId: String,
  val player: Player,
  val opponent: Player,
  val startingPlayer: Player,
  val connection: GameSocket
) {
  val piecesVar: Var[Map[Location, Piece]] = Var(Map())
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val treasuresVar: Var[Set[Location]] = Var(Set())
  val selectedPiece: Var[Option[(Option[Location], Piece)]] = Var(None)

  val playerGoldVar: Var[Int] = Var(100)
  val opponentGoldVar: Var[Int] = Var(100)

  val playerClockVar = Var(5.minutes)
  val opponentClockVar = Var(5.minutes)

  connection.socket.addEventListener("message", { event: MessageEvent =>
    val message: GameMessage = read[GameMessage](event.data.toString)
    println(s"Received game message: $message")
    message match {
      case InitializeBoard(board, pieces, treasures) => {
        println("Initializing board")
        boardVar.set(Some(board.squares))
        piecesVar.set(pieces)
        treasuresVar.set(treasures)
      }
      case UpdatePlayerClocks(clocks) => {
        clocks.get(player).foreach { clock =>
          playerClockVar.update(_ => clock.remaining)
        }
        clocks.get(opponent).foreach { clock =>
          opponentClockVar.update(_ => clock.remaining)  
        }
      }
      case UpdateBoardState(nextActivePlayer, pieces, gold, treasures) => {
        val playerToMove = playerToMoveSignal
        piecesVar.set(pieces)
        treasuresVar.set(treasures)
        if (nextActivePlayer == player) {
          moveTurnBus.emit() // TODO: take a closer look here.  moveTurnBus.emit() is called in GameEvents which sends the move over to the opponent, and this brings it back to the player.  So it "works out".  But maybe moveTurnBus should keep track of players more explicitly.
        }
        gold.get(player).foreach { updatedGold =>
          playerGoldVar.set(updatedGold)
        }
        gold.get(opponent).foreach { updatedGold =>
          opponentGoldVar.set(updatedGold)
        }
      }
      case _ =>
    }
  })

  val moveTurnBus = new EventBus[Unit]
  val playerToMoveSignal: Signal[Player] = moveTurnBus.events.scanLeft(startingPlayer) { case (p, _) =>
    if (p == player) opponent else player 
  }

  val isPlayerTurnSignal: Signal[Boolean] = playerToMoveSignal.map { p =>
    if (p == player) true else false
  }

  val boardStateSignal = Signal.combine(boardVar.signal, piecesVar.signal, treasuresVar.signal)

  val validMovesSignal = Signal.combine(boardStateSignal, selectedPiece.signal).map {
    case ((Some(board), pieces, treasures), Some((Some(location), piece))) => {
      validMoves(BoardWithPieces(board, pieces, treasures), location, piece)
    }
    case ((Some(board), pieces, treasures), Some((None, piece))) => {
      validPiecePlacements(BoardWithPieces(board, pieces, treasures), piece)
    }
    case _ => Set.empty[Location]
  }

  val validCapturesSignal = Signal.combine(boardStateSignal, selectedPiece.signal).map {
    case ((Some(board), pieces, treasures), Some((Some(location), piece))) => {
      validCaptures(BoardWithPieces(board, pieces, treasures), location, piece)
    }
    // can a piece be placed onto another piece, thus capturing it?
    case _ => Set.empty[Location]
  }

  val playerGoldSignal = playerGoldVar.signal

  val gameStateSignal = Signal.combine(boardStateSignal, playerGoldVar.signal, opponentGoldVar.signal, playerToMoveSignal).map {
    case ((board, pieces, treasures), playerGold, opponentGold, playerToMove) =>
      val white = if (player.color == White) player else opponent
      val black = if (player.color == Black) player else opponent

      val whiteGold = if (player.color == White) playerGold else opponentGold
      val blackGold = if (player.color == Black) playerGold else opponentGold

      val gold = Map(white -> whiteGold, black -> blackGold)

      GameState(
        gameId,
        white,
        black,
        Board(board.getOrElse(Vector())),
        pieces,
        gold,
        treasures,
        playerToMove
      )
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

  val assignPlayers: MessageEvent => Unit = event => {
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

  connection.socket.addEventListener("message", assignPlayers)
}
