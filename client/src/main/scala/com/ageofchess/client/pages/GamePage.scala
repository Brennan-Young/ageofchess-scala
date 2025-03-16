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

class GamePageClass(val gameId: String) { // todo: rename to GamePage

   // TODO: Might be better as a Vector[Vector[(Location, Var[RenderablePiece])]], as a grid.  
   // We'd be storing a bunch more in memory, but each square can evolve independently rather 
   // than updating the entire Var every time a piece moves.
  private val piecesVar: Var[Map[Location, RenderablePiece]] = Var(Map())
  private val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)

  private val socket = new Sockets.GameSocket(gameId).socket

  // val player1: Var[Player] = Var(None)
  // val player2: Var[Player] = Var(None)

  socket.onopen = _ => {
    socket.send("new connection")
  }

  socket.onmessage = event => {
    val message: GameMessage = read[GameMessage](event.data.toString)
    println(s"Received message: $message")
    message match {
      case InitializeBoard(board, pieces) => {
        boardVar.set(Some(board.squares))
        piecesVar.set(pieces.toMap)
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
        case Some(b) => Rendering.renderState2(b, piecesVar)
        case None => div("Loading")
      },
      a(href := "/", "Back to Home")
    )
  }
}

object GamePage {
  val piecesVar: Var[Map[Location, RenderablePiece]] = Var(Map())

  Sockets.gameStateSocket.onmessage = event => {
    val updatedPieces = read[Map[Location, RenderablePiece]](event.data.toString)
    piecesVar.set(updatedPieces)
  }

  def render(implicit ec: ExecutionContext): Div = {

    val boardStateSignal: Signal[Option[Vector[Vector[(RenderableSquare, Option[RenderablePiece])]]]] = boardState(piecesVar)
    
    div(
      h1("Game Board"),
      child <-- boardStateSignal.map {
        case Some(b) => Rendering.renderState(b, piecesVar)
        case None => div("Loading")
      },
      a(href := "/", "Back to Home")
    )
  }

  def boardState(piecesVar: Var[Map[Location, RenderablePiece]])(implicit ec: ExecutionContext): Signal[Option[Vector[Vector[(RenderableSquare, Option[RenderablePiece])]]]] = {
    val board: Future[Vector[Vector[RenderableSquare]]] = Queries.fetchBoard()

    val boardVar: Var[Option[Vector[Vector[RenderableSquare]]]] = Var(None)

    board.foreach { renderableBoard => boardVar.set(Some(renderableBoard)) }
    piecesVar.set(defaultPieces)

    Signal.combine(boardVar.signal, piecesVar.signal).map {
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
}
