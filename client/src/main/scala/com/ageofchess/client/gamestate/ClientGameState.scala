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
      // case UpdatePieces(nextActivePlayer, pieces) => {
      //   println("Updating board state")
      //   piecesVar.set(pieces)
      // }
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

  // val boardSignal = Signal.combine(boardVar.signal, piecesVar.signal, validMovesSignal).map {
  //   case (Some(board), pieces, validMovesOfSelection) =>
  //     Some((board, pieces, validMovesOfSelection))
  //   case _ => None
  // }

  val boardSignal = Signal.combine(boardVar.signal, piecesVar.signal).map {
    case (Some(board), pieces) =>
      Some((board, pieces, Set.empty[Location]))
    case _ => None
  }

  // val boardSignal = Signal.combine(boardVar.signal, piecesVar.signal, selectedPiece.signal).map {
  //   case (Some(board), pieces, Some((location, piece))) => {
  //     Some((board, pieces, validMoves(BoardWithPieces(board, pieces), location, piece)))
  //   }
  //   case (Some(board), pieces, None) => {
  //     Some((board, pieces, Set.empty[Location]))
  //   }
  //   case _ => None
  // }

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

// class ClientGameState(val gameId: String) {
//   // TODO: Might be better as a Vector[Vector[(Location, Var[Piece])]], as a grid.  
//   // We'd be storing a bunch more in memory, but each square can evolve independently rather 
//   // than updating the entire Var every time a piece moves.
//   val piecesVar: Var[Map[Location, Piece]] = Var(Map())
//   val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
//   val playerVar: Var[Option[Player]] = Var(None)
//   val opponentVar: Var[Option[Player]] = Var(None)
//   val playerToMoveVar: Var[Option[Player]] = Var(None)
//   val selectedPiece: Var[Option[Location]] = Var(None)

//   // TODO: Not part of the game state, it's just how the client communicates with the server.
//   // Should be moved out of this class or the class should be renamed
//   val socket = Sockets.buildSocket(gameId)

//   socket.onopen = _ => {
//     socket.send(write(ConnectPlayer("placeholder")))
//   }

//   socket.onmessage = event => {
//     val message: GameMessage = read[GameMessage](event.data.toString)
//     println(s"Received message: $message")
//     message match {
//       case InitializeBoard(board, pieces) => {
//         println("Initializing board")
//         boardVar.set(Some(board.squares))
//         piecesVar.set(pieces)
//       }
//       case AssignPlayers(player, opponent) => {
//         println("Assigning players")
//         playerVar.set(Some(player))
//         opponentVar.set(Some(opponent))
//         val startingPlayer = if (player.color == White) player else opponent
//         // TODO: obviously bad code, fix this
//         if (startingPlayer == player) moveTurnBus.emit() else {
//           moveTurnBus.emit()
//           moveTurnBus.emit()
//         }
//       }
//       case UpdatePieces(pieces) => {
//         println("Updating board state")
//         piecesVar.set(pieces)
//       }
//       case _ =>
//     }
//   }

//   val boardStateSignal = Signal.combine(boardVar.signal, piecesVar.signal).map {
//     case (Some(board), pieces) => {
//       println("a")
//       val zippedBoard = board.zipWithIndex.map { case (row, rIdx) =>
//         row.zipWithIndex.map { case (square, cIdx) =>
//           val pieceOpt = pieces.get(Location(rIdx, cIdx))
//           (square, pieceOpt)
//         }  
//       }
//       Some(zippedBoard)
//     }
//     case _ => {println("b") ; None }
//   }

//   val moveTurnBus = new EventBus[Unit]
//   val playerToMoveSignal: Signal[Option[Player]] = moveTurnBus.events.scanLeft(None: Option[Player]) { case (player, _) =>
//     if (player == playerVar.now()) opponentVar.now() else playerVar.now()  
//   }

//   val isPlayerTurnSignal: Signal[Boolean] = Signal.combine(playerVar.signal, playerToMoveSignal).map {
//     case (Some(player), Some(playerToMove)) => if (player == playerToMove) true else false
//     case _ => false
//   }
// }
