package com.ageofchess.client.board

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Messages._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.client.api.Sockets
import com.ageofchess.client.gamestate.ClientGameState
import org.scalajs.dom

import upickle.default._ // remove later

class GameStateRenderer(val gameState: ClientGameState) {
  def render(
    boardState: Vector[Vector[(SquareType, Option[RenderablePiece])]]
  ): HtmlElement = {
    val numColumns = boardState.headOption.map(_.size).getOrElse(0)

    div(
      cls := "board",
      styleAttr := s"grid-template-columns: repeat(${numColumns}, 50px);",
      boardState.zipWithIndex.map { case (row, rIdx) =>
        div(
          cls := "board-row",
          row.zipWithIndex.map { case ((square, piece), cIdx) =>
            val squareColor = if ((rIdx + cIdx) % 2 == 0) Grass else Dirt
            val renderableSquare = RenderableSquare(squareColor, square)
            renderSquare(Location(rIdx, cIdx), renderableSquare, piece, gameState.piecesVar)  
          }
        )  
      }
    )
  }

  def renderSquare(
    location: Location,
    square: RenderableSquare,
    piece: Option[RenderablePiece],
    piecesVar: Var[Map[Location, RenderablePiece]]
  ): HtmlElement = {

    div(
      cls := "board-square",
      backgroundImage := s"url(/assets/${square.asset})",
      piece.map { p =>
        img(
          src := s"/assets/pieces/${p.asset}",
          cls := "piece",
          onDragStart --> { _ =>
            gameState.selectedPiece.set(Some(location))
          }
        )
      },
      onClick --> { movePieceOnClick(location, piece, piecesVar) },
      onDragOver.preventDefault --> { _ => },
      onDrop.preventDefault --> { _ =>
        gameState.selectedPiece.now() match {
          case Some((selectedPosition)) => {
            gameState.socket.send(write(MovePiece(selectedPosition, location)))
            piecesVar.update { pieces =>
              pieces.get(selectedPosition).map { pieceToMove =>
                pieces - selectedPosition + (location -> pieceToMove)  
              }.getOrElse(pieces)
            }
            gameState.selectedPiece.set(None)
          }
          case None => 
        }
      }
    )
  }

  def movePieceOnClick(
    location: Location,
    piece: Option[RenderablePiece],
    piecesVar: Var[Map[Location, RenderablePiece]]
  ): dom.Event => Unit = { _ =>
    gameState.selectedPiece.now() match {
      case Some(selectedPosition) if selectedPosition != location => {
        piecesVar.update { pieces =>
          pieces.get(selectedPosition).map { pieceToMove =>
            pieces - selectedPosition + (location -> pieceToMove)
          }.getOrElse(pieces) 
        }
        gameState.socket.send(write(MovePiece(selectedPosition, location)))
        gameState.selectedPiece.set(None)
      }
      case Some(_) => gameState.selectedPiece.set(None)
      case None => piece match {
        case Some(_) => gameState.selectedPiece.set(Some(location))
        case None =>
      }
    }

  }
}
