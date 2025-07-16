package com.ageofchess.client.events

import com.ageofchess.client.gamestate.ClientGame
import com.raquo.laminar.api.L._
import org.scalajs.dom
import com.ageofchess.shared.piece._
import com.ageofchess.shared.Messages._
import upickle.default._

object GameEvents {
  
  val mouseDragStartBus = new EventBus[(dom.DragEvent, Location, Piece)]
  val mouseDragDropBus = new EventBus[Location]
  val mouseClickBus = new EventBus[(Option[(dom.MouseEvent, Location)])]

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

  def mouseDragDropEvents(
    dropBus: EventBus[Location],
    isValidMoveOfCurrentSelectionSignal: Signal[Boolean],
    selectedPieceSignal: Signal[Option[(Location, Piece)]],
    location: Location
  ) = {
    dropBus
      .events
      .withCurrentValueOf(isValidMoveOfCurrentSelectionSignal, selectedPieceSignal)
      .filter { case (toLoc, isValid, selectedPiece) => toLoc == location }
  }

  def mouseDragDropEffects(gameState: ClientGame) = Observer[(Location, Boolean, Option[(Location, Piece)])](onNext = { case (toLoc, isValidMove, selectedPiece) =>
    selectedPiece match {
      case Some((fromLoc, piece)) if isValidMove => movePiece(gameState, fromLoc, toLoc)
      case _ =>
    }
  })

  def mouseClickEvents(
    clickBus: EventBus[Option[(dom.MouseEvent, Location)]],
    gameState: ClientGame
  ): EventStream[(dom.MouseEvent, Location, Color, Boolean, Option[Piece], Set[Location], Set[Location])] = {

    clickBus
      .events
      .withCurrentValueOf(gameState.isPlayerTurnSignal, gameState.piecesVar.signal, gameState.validMovesSignal, gameState.validCapturesSignal)
      .collect {
        // if the square we've selected has a piece in it, or if we've currently selected a piece - 
        // that is, discard the clicks where a player clicks an empty square with no selected piece
        case (Some((event, location)), isPlayerTurn, pieces, validMoves, validCaptures) if (
          (pieces.contains(location) && !gameState.selectedPiece.now().isDefined) ||
          (gameState.selectedPiece.now().isDefined)
        ) =>
          (event, location, gameState.player.color, isPlayerTurn, pieces.get(location), validMoves, validCaptures)
      }
  }

  def mouseClickEffects(
    gameState: ClientGame
  ) = Observer[(dom.MouseEvent, Location, Color, Boolean, Option[Piece], Set[Location], Set[Location])](onNext = { case (e, loc, playerColor, isPlayerTurn, piece, validMoves, validCaptures) =>
    if (isPlayerTurn) {
      gameState.selectedPiece.now() match {
        case Some((position, _)) if (validMoves.contains(loc) || validCaptures.contains(loc)) => movePiece(gameState, position, loc)
        case Some((position, _)) if loc == position => gameState.selectedPiece.set(None) // deselect current piece
        case _ => {
          // covers case where player has a piece selected and selects a piece of the same color or no piece is selected
          // piece should be defined if selectedPiece is None as clickEvents filtered out cases where this doesn't hold.
          // no-op if no piece is defined
          piece.foreach { p =>
            if (p.color == playerColor) gameState.selectedPiece.set(Some(loc, p))
          }
        }
      }
    } else {
      // Perhaps should also set the current piece to None
      e.preventDefault()
    }
  })

  def movePiece( 
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
