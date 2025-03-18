package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.client.board.Rendering
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.game._
import scala.concurrent.Future
import upickle.default._
import scala.concurrent.ExecutionContext
import org.scalajs.dom
import com.ageofchess.client.api.Queries
import com.ageofchess.client.api.Sockets
import com.ageofchess.shared.Messages._

class GamePage(val gameId: String) {

   // TODO: Might be better as a Vector[Vector[(Location, Var[RenderablePiece])]], as a grid.  
   // We'd be storing a bunch more in memory, but each square can evolve independently rather 
   // than updating the entire Var every time a piece moves.
   // TODO: Can these be private?  Currently no as GameRenderer needs to make use of these Vars.  
  val piecesVar: Var[Map[Location, RenderablePiece]] = Var(Map())
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val playerVar: Var[Option[Player]] = Var(None)
  val opponentVar: Var[Option[Player]] = Var(None)
  val playerToMoveVar: Var[Option[Player]] = Var(None) 

  val socket = new Sockets.GameSocket(gameId).socket

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

  def render(implicit ec: ExecutionContext): Div = {
    div(
      h1("Game Board"),
      child <-- boardStateSignal.map {
        case Some(b) => Rendering.renderState(b, piecesVar)
        case None => div("Loading")
      },
      a(href := "/", "Back to Home")
    )
  }
}

// object GamePage {
//   val piecesVar: Var[Map[Location, RenderablePiece]] = Var(Map())

//   Sockets.gameStateSocket.onmessage = event => {
//     val updatedPieces = read[Map[Location, RenderablePiece]](event.data.toString)
//     piecesVar.set(updatedPieces)
//   }

//   def render(implicit ec: ExecutionContext): Div = {

//     val boardStateSignal: Signal[Option[Vector[Vector[(RenderableSquare, Option[RenderablePiece])]]]] = boardState(piecesVar)
    
//     div(
//       h1("Game Board"),
//       child <-- boardStateSignal.map {
//         case Some(b) => Rendering.renderState(b, piecesVar)
//         case None => div("Loading")
//       },
//       a(href := "/", "Back to Home")
//     )
//   }

//   def boardState(piecesVar: Var[Map[Location, RenderablePiece]])(implicit ec: ExecutionContext): Signal[Option[Vector[Vector[(RenderableSquare, Option[RenderablePiece])]]]] = {
//     val board: Future[Vector[Vector[RenderableSquare]]] = Queries.fetchBoard()

//     val boardVar: Var[Option[Vector[Vector[RenderableSquare]]]] = Var(None)

//     board.foreach { renderableBoard => boardVar.set(Some(renderableBoard)) }
//     piecesVar.set(defaultPieces)

//     Signal.combine(boardVar.signal, piecesVar.signal).map {
//       case (Some(board), pieces) => {
//         val zippedBoard = board.zipWithIndex.map { case (row, rIdx) =>
//           row.zipWithIndex.map { case (square, cIdx) =>
//             val pieceOpt = pieces.get(Location(rIdx, cIdx))
//             (square, pieceOpt)
//           }  
//         }
//         Some(zippedBoard)
//       }
//       case _ => None
//     }
//   }
// }
