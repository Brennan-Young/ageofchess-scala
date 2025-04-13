package com.ageofchess.client.events

import com.ageofchess.client.gamestate.ClientGame
import com.raquo.laminar.api.L._
import org.scalajs.dom
import com.ageofchess.shared.piece._
import com.ageofchess.shared.Messages._
import upickle.default._

object GameEvents {
  
  def mouseDragStartBus = new EventBus[(dom.DragEvent, Location, Piece)]
  def mouseDragDropBus = new EventBus[Location]
  def mouseClickBus = new EventBus[(Option[(dom.MouseEvent, Location)])]

  def mouseDragStartEvents(
    dragBus: EventBus[(dom.DragEvent, Location, Piece)],
    gameState: ClientGame
  ): EventStream[(dom.DragEvent, Location, Piece, Boolean)] = {

    dragBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)  
  }

  def mouseDragStartEffects(gameState: ClientGame) = Observer[(dom.DragEvent, Location, Piece, Boolean)](onNext = 
    {
      case (event, loc, piece, canMove) =>
        if (canMove && piece.color == gameState.player.color) {
          gameState.selectedPiece.set(Some(loc, piece))
        } else {
          event.preventDefault()
        }
    }
  )

  def mouseClickEvents(
    clickBus: EventBus[Option[(dom.MouseEvent, Location)]],
    gameState: ClientGame
  ): EventStream[(dom.MouseEvent, Location, Boolean, Option[Piece])] = {

    clickBus
      .events
      .withCurrentValueOf(gameState.isPlayerTurnSignal, gameState.piecesVar.signal)
      .collect {
        // if the square we've selected has a piece in it, or if we've currently selected a piece - 
        // that is, discard the clicks where a player clicks an empty square with no selected piece
        case (Some((event, location)), canMove, pieces) if (pieces.contains(location) || gameState.selectedPiece.now().isDefined) =>
          (event, location, canMove, pieces.get(location))
      }
  }

  def mouseClickEffects(
    gameState: ClientGame
  ) = Observer[(dom.MouseEvent, Location, Boolean, Option[Piece])](onNext = { case (e, loc, canMove, piece) =>
    if (canMove) {
      gameState.selectedPiece.now() match {
        case Some((position, piece)) => drop(gameState, position, loc)
        // TODO: piece should exist if selectedPiece is None as clickEvents filtered out cases where this doesn't hold.
        // But should find a way to express better
        case None => gameState.selectedPiece.set(Some(loc, piece.get))
      }
    } else {
      // Perhaps should also set the current piece to None
      e.preventDefault()
    }
  })

  def drop(
    gameState: ClientGame,
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
