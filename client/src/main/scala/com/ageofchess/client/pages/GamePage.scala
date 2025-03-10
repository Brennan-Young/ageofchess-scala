package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.client.board.Rendering
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import scala.concurrent.Future
import upickle.default._
import scala.concurrent.ExecutionContext
import org.scalajs.dom
import com.ageofchess.client.api.Queries
import com.ageofchess.client.api.Sockets

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
