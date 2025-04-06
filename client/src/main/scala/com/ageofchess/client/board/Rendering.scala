package com.ageofchess.client.board

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Messages._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.client.api.Sockets
import com.ageofchess.client.gamestate.ClientGame
import org.scalajs.dom
import scala.util.Try
import upickle.default._ // remove later

class GameStateRenderer(val gameState: ClientGame) {
  def render(
    boardState: Vector[Vector[(SquareType, Option[Piece])]]
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
            renderSquare(Location(rIdx, cIdx), renderableSquare, piece)  
          }
        )
      },
      onClick.map { event => findSquare(event.target).map(event -> _) } --> clickBus.writer,
      clickEvents --> clickEffects,
      onMountCallback { ctx =>
        gameState.connection.socket.onmessage = event => {
          val message: GameMessage = read[GameMessage](event.data.toString)
          val playerToMove = gameState.playerToMoveSignal.observe(ctx.owner)
          println(s"Received game message: $message")
          message match {
            case UpdatePieces(nextActivePlayer, pieces) => {
              gameState.piecesVar.set(pieces)
              if (nextActivePlayer == gameState.player && playerToMove.now() != gameState.player) {
                gameState.moveTurnBus.emit()
              }
            }
            case _ =>
          }
        }
      }
    )
  }

  def findSquare(target: dom.EventTarget): Option[Location] = {
    target match {
      case el: dom.Element => {
        el.closest(".board-square") match {
          case null => None
          case square => for {
            x <- Try(square.getAttribute("data-x").toInt).toOption
            y <- Try(square.getAttribute("data-y").toInt).toOption
          } yield Location(x, y)
        }
      }
    }
  }

  val dragBus = new EventBus[(dom.DragEvent, Location, Piece)]
  val dragEvents = dragBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)
  val dragEffects = Observer[(dom.DragEvent, Location, Piece, Boolean)](onNext = { case (e, loc, piece, canMove) =>
    if (canMove) {
      gameState.selectedPiece.set(Some(loc, piece))
    } else {
      e.preventDefault()
    }
  })

  val clickBus = new EventBus[Option[(dom.MouseEvent, Location)]]

  val clickEvents = clickBus
    .events
    .withCurrentValueOf(gameState.isPlayerTurnSignal, gameState.piecesVar.signal)
    .collect {
      // if the square we've selected has a piece in it, or if we've currently selected a piece - 
      // that is, discard the clicks where a player clicks an empty square with no selected piece
      case (Some((event, location)), canMove, pieces) if (pieces.contains(location) || gameState.selectedPiece.now().isDefined) =>
        (event, location, canMove, pieces.get(location))
    }

  val clickEffects = Observer[(dom.MouseEvent, Location, Boolean, Option[Piece])](onNext = { case (e, loc, canMove, piece) =>
    if (canMove) {
      gameState.selectedPiece.now() match {
        case Some((position, piece)) => drop(position, loc)
        // TODO: piece should exist if selectedPiece is None as clickEvents filtered out cases where this doesn't hold.
        // But should find a way to express better
        case None => gameState.selectedPiece.set(Some(loc, piece.get))
      }
    } else {
      // Perhaps should also set the current piece to None
      e.preventDefault()
    }
  })

  def renderSquare(
    location: Location,
    square: RenderableSquare,
    piece: Option[Piece]
  ): HtmlElement = {
    div(
      cls := "board-square",
      dataAttr("x") := location.x.toString,
      dataAttr("y") := location.y.toString,
      backgroundImage := s"url(/assets/${square.asset})",
      piece.map { p =>
        img(
          src := s"/assets/pieces/${p.asset}",
          cls := "piece",
          // TODO: this doesn't work remotely as intended, need to read up on animations more
          // styleAttr := s"transform: translate(${location.y * 50}px, ${location.x}px);",
          onDragStart.map(e => (e, location, p)) --> dragBus.writer,
          dragEvents --> dragEffects
        )
      },
      onDragOver.preventDefault --> { _ => },
      onDrop.preventDefault --> { _ =>
        gameState.selectedPiece.now().foreach { case (position, piece) =>
          drop(position, location)
        }
      }
    )
  }

  def drop(
    originalPosition: Location,
    newPosition: Location
  ): Unit = {
    if (newPosition != originalPosition) {
      gameState.piecesVar.update { pieces =>
        pieces.get(originalPosition).map { pieceToMove =>
          pieces - originalPosition + (newPosition -> pieceToMove)
        }.getOrElse(pieces)
      }
      gameState.connection.socket.send(write(MovePiece(gameState.player, originalPosition, newPosition)))
      gameState.selectedPiece.set(None)
      gameState.moveTurnBus.emit()
    } 
    else {
      gameState.selectedPiece.set(None)
    }
  }
}
