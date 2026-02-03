package com.ageofchess.client.events

import com.ageofchess.client.gamestate.PlayerGameView
import com.ageofchess.client.board.{AnimatingMove, MoveAnimation}
import com.raquo.laminar.api.L._
import org.scalajs.dom
import com.ageofchess.shared.piece._
import com.ageofchess.shared.Messages._
import com.ageofchess.shared.game._
import upickle.default._
import com.ageofchess.client.api.Sockets.GameSocket

object GameEvents {
  
  val mouseDragStartBus = new EventBus[(dom.DragEvent, Option[Location], Piece)]
  val mouseDragDropBus = new EventBus[Location]
  val mouseClickBus = new EventBus[(Option[(dom.MouseEvent, Location)])]

  def mouseDragStartEvents(
    dragBus: EventBus[(dom.DragEvent, Option[Location], Piece)],
    gameState: PlayerGameView
  ): EventStream[(dom.DragEvent, Option[Location], Piece, Boolean)] = {

    dragBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)  
  }

  def mouseDragStartEffects(gameState: PlayerGameView) = Observer[(dom.DragEvent, Option[Location], Piece, Boolean)](onNext = 
    {
      case (event, loc, piece, isPlayerTurn) =>
        if (isPlayerTurn && piece.color == gameState.player.color) {
          gameState.selectedPiece.set(Some(loc, piece))
        } else {
          event.preventDefault()
        }
    }
  )

  def mouseDragDropEvents(
    dropBus: EventBus[Location],
    clientGame: PlayerGameView,
    isValidMoveOfCurrentSelectionSignal: Signal[Boolean],
    location: Location
  ) = {
    dropBus
      .events
      .withCurrentValueOf(clientGame.gameStateSignal, clientGame.selectedPiece.signal, clientGame.playerGoldSignal, isValidMoveOfCurrentSelectionSignal)
      .filter { case (toLoc, _, _, _, _) => toLoc == location }
  }

  def mouseDragDropEffects(
    clientGame: PlayerGameView
  ) = {
    Observer[(Location, GameState, Option[(Option[Location], Piece)], Int, Boolean)](onNext = {
      case (draggedToLocation, gameState, selectedPiece, playerGold, isValidMove) =>
        val playerAction: Option[PlayerAction] = selectedPiece match {
          case Some((Some(draggedFromLocation), piece)) if isValidMove => Some(PieceMove(piece, draggedFromLocation, draggedToLocation))
          case Some((None, piece)) if isValidMove && playerGold >= piece.pieceType.value => {
            println("placing piece")
            Some(PiecePlacement(piece, draggedToLocation))
          }
          case Some((None, piece)) => {
            clientGame.selectedPiece.set(None)
            // TODO: differentiate between invalid piece placement and insufficient gold
            None // TODO: display insufficient gold message
          }
          case _ => {
            println(selectedPiece, isValidMove)
            None // display invalid move message?
          }
        }

        selectedPiece match {
          case Some((Some(draggedFromLocation), piece)) => {
            updateMoveAnimation(piece, draggedFromLocation, draggedToLocation, clientGame)
          }
          case _ =>
        }

        for {
          action <- playerAction
          nextState <- gameState.validateAndGenerateNextState(clientGame.player, action)
        } {
          val message: ClientMessage = action match {
            case PieceMove(piece, f, t) => MovePiece(clientGame.player, piece, f, t)
            case PiecePlacement(p, t) => PlacePiece(clientGame.player, p, t)
          }

          updateGameStateVariables(clientGame, nextState)
          sendGameStateToServer(clientGame.connection, message)
        }
    })
  }

  def mouseClickEvents(
    clickBus: EventBus[Option[(dom.MouseEvent, Location)]],
    clientGame: PlayerGameView
  ): EventStream[(dom.MouseEvent, Location, GameState, Boolean, Option[Piece], Set[Location], Set[Location], Option[(Option[Location], Piece)])] = {

    clickBus
      .events
      .withCurrentValueOf(clientGame.gameStateSignal, clientGame.isPlayerTurnSignal, clientGame.validMovesSignal, clientGame.validCapturesSignal, clientGame.selectedPiece.signal)
      .collect {
        // if the square we've selected has a piece in it, or if we've currently selected a piece - 
        // that is, discard the clicks where a player clicks an empty square with no selected piece
        case (Some((event, location)), gameState, isPlayerTurn, validMoves, validCaptures, selectedPiece) if (
          (gameState.pieces.contains(location) && !selectedPiece.isDefined) ||
          (selectedPiece.isDefined)
        ) =>
          (event, location, gameState, isPlayerTurn, gameState.pieces.get(location), validMoves, validCaptures, selectedPiece)
      }
  }

  def mouseClickEffects(
    clientGame: PlayerGameView
  ) = Observer[(dom.MouseEvent, Location, GameState, Boolean, Option[Piece], Set[Location], Set[Location], Option[(Option[Location], Piece)])](onNext = {
    case (e, clickedLocation, gameState, isPlayerTurn, piece, validMoves, validCaptures, selectedPiece) =>
      if (isPlayerTurn) {
        selectedPiece match {
          case Some((Some(currentPosition), clickedPiece)) if (validMoves.contains(clickedLocation) || validCaptures.contains(clickedLocation)) => {
            println(s"Moving piece from $currentPosition to $clickedLocation")
            val action = PieceMove(clickedPiece, currentPosition, clickedLocation)
            val nextState = gameState.validateAndGenerateNextState(clientGame.player, action)

            updateMoveAnimation(clickedPiece, currentPosition, clickedLocation, clientGame)

            nextState.foreach { state =>
              updateGameStateVariables(clientGame, state)
              sendGameStateToServer(clientGame.connection, MovePiece(clientGame.player, clickedPiece, currentPosition, clickedLocation))
            }
          }
          case Some((Some(currentPosition), _)) if clickedLocation == currentPosition => {
            println(s"Deselecting piece at $currentPosition")
            clientGame.selectedPiece.set(None) // deselect current piece
          }
          case Some((None, _)) => {
            // not expected to be a reachable state as placing pieces is only supported with drag at the moment
            println(s"No-op with click $clickedLocation")
          }
          case _ => {
            // covers case where player has a piece selected and selects a piece of the same color or no piece is selected
            // piece should be defined if selectedPiece is None as clickEvents filtered out cases where this doesn't hold.
            // no-op if no piece is defined
            piece.foreach { p =>
              println(s"Selecting piece at $clickedLocation")
              if (p.color == clientGame.player.color) clientGame.selectedPiece.set(Some((Some(clickedLocation), p)))
            }
          }
        }
      } else {
        // Perhaps should also set the current piece to None
        e.preventDefault()
      }
  })

  def updateMoveAnimation(
    piece: Piece,
    from: Location,
    to: Location,
    clientGameState: PlayerGameView
  ): Unit = {
    val moveAnimation = AnimatingMove(piece, from, to)
    clientGameState.moveAnimationVar.set(Some(moveAnimation))
  }

  def updateGameStateVariables(
    clientGameState: PlayerGameView,
    nextGameState: GameState
  ): Unit = {
    

    val oldPieces = clientGameState.piecesVar.now()
    val newMoves  = MoveAnimation.inferMoves(oldPieces, nextGameState.pieces)
    clientGameState.animatingMovesVar.update(list => list ++ newMoves)
    clientGameState.piecesVar.update(pieces => nextGameState.pieces)
    clientGameState.playerGoldVar.update(gold => nextGameState.gold.get(clientGameState.player).getOrElse(0)) // TODO: Difficulty arises from player gold sometimes being stored as a Map versus sometimes two named variables.  May want to make this consistent
    clientGameState.treasuresVar.update(treasures => nextGameState.treasures)
    clientGameState.selectedPiece.set(None)
    clientGameState.moveTurnBus.emit()
  }

  def sendGameStateToServer(
    socket: GameSocket,
    message: ClientMessage
  ): Unit = {
    socket.socket.send(write(message))
  }
}
