package com.ageofchess.client.events

import com.ageofchess.client.gamestate.ClientGame
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
    gameState: ClientGame
  ): EventStream[(dom.DragEvent, Option[Location], Piece, Boolean)] = {

    dragBus.events.withCurrentValueOf(gameState.isPlayerTurnSignal)  
  }

  def mouseDragStartEffects(gameState: ClientGame) = Observer[(dom.DragEvent, Option[Location], Piece, Boolean)](onNext = 
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
    isValidMoveOfCurrentSelectionSignal: Signal[Boolean],
    selectedPieceSignal: Signal[Option[(Option[Location], Piece)]],
    playerGoldSignal: Signal[Int],
    location: Location
  ) = {
    dropBus
      .events
      .withCurrentValueOf(isValidMoveOfCurrentSelectionSignal, selectedPieceSignal, playerGoldSignal)
      .filter { case (toLoc, isValid, selectedPiece, playerGold) => toLoc == location }
  }

  def mouseDragDropEffects(gameState: ClientGame) = Observer[(Location, Boolean, Option[(Option[Location], Piece)], Int)](onNext = { case (toLoc, isValidMove, selectedPiece, playerGold) =>
    selectedPiece match {
      case Some((Some(fromLoc), piece)) if isValidMove => movePiece(gameState, fromLoc, toLoc)
      case Some((None, piece)) if isValidMove && playerGold >= piece.pieceType.value => placePiece(gameState, piece, toLoc)
      case Some((None, piece)) if isValidMove => // TODO: display insufficient gold message
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

  def mouseClickEv(
    clickBus: EventBus[Option[(dom.MouseEvent, Location)]],
    clientGame: ClientGame
  ): EventStream[(dom.MouseEvent, Location, GameState, Boolean, Option[Piece], Set[Location], Set[Location], Option[(Option[Location], Piece)])] = {

    clickBus
      .events
      .withCurrentValueOf(clientGame.gameStateSignal, clientGame.isPlayerTurnSignal, clientGame.validMovesSignal, clientGame.validCapturesSignal, clientGame.selectedPiece.signal)
      .collect {
        case (Some((event, location)), gameState, isPlayerTurn, validMoves, validCaptures, selectedPiece) if (
          (gameState.pieces.contains(location) && !selectedPiece.isDefined) ||
          (selectedPiece.isDefined)
        ) =>
          (event, location, gameState, isPlayerTurn, gameState.pieces.get(location), validMoves, validCaptures, selectedPiece)
      }
  }

  def mouseClickEff(
    clientGame: ClientGame
  ) = Observer[(dom.MouseEvent, Location, GameState, Boolean, Option[Piece], Set[Location], Set[Location], Option[(Option[Location], Piece)])](onNext = {
    case (e, clickedLocation, gameState, isPlayerTurn, piece, validMoves, validCaptures, selectedPiece) =>
      if (isPlayerTurn) {
        selectedPiece match {
          case Some((Some(currentPosition), _)) if (validMoves.contains(clickedLocation) || validCaptures.contains(clickedLocation)) => {
            println(s"Moving piece from $currentPosition to $clickedLocation")
            // movePiece(gameState, currentPosition, clickedLocation)
            val action = PieceMove(currentPosition, clickedLocation)
            val nextState = gameState.computeNextState(action)

            println(s"State update: ${gameState}, ${nextState}")

            nextState.foreach { state =>
              updateGameStateVariables(clientGame, state)
              sendGameStateToServer(clientGame.connection, MovePiece(clientGame.player, currentPosition, clickedLocation))
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
      }
  })

  def mouseClickEffects(
    gameState: ClientGame
  ) = Observer[(dom.MouseEvent, Location, Color, Boolean, Option[Piece], Set[Location], Set[Location])](onNext = { case (e, clickedLocation, playerColor, isPlayerTurn, piece, validMoves, validCaptures) =>
    if (isPlayerTurn) {
      gameState.selectedPiece.now() match {
        case Some((Some(currentPosition), _)) if (validMoves.contains(clickedLocation) || validCaptures.contains(clickedLocation)) => {
          println(s"Moving piece from $currentPosition to $clickedLocation")
          movePiece(gameState, currentPosition, clickedLocation)
        }
        case Some((Some(currentPosition), _)) if clickedLocation == currentPosition => {
          println(s"Deselecting piece at $currentPosition")
          gameState.selectedPiece.set(None) // deselect current piece
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
            if (p.color == playerColor) gameState.selectedPiece.set(Some((Some(clickedLocation), p)))
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

      if (gameState.treasuresVar.now().contains(newPosition)) {
        gameState.playerGoldVar.update(gold => gold + TreasureValue)
        gameState.treasuresVar.update(treasures => treasures - newPosition)
      }

      gameState.connection.socket.send(write(MovePiece(gameState.player, originalPosition, newPosition)))
      gameState.selectedPiece.set(None)
      gameState.moveTurnBus.emit()
    } 
    else {
      gameState.selectedPiece.set(None)
    }
  }

  def placePiece(
    gameState: ClientGame,
    piece: Piece,
    location: Location,
  ): Unit = {
    gameState.piecesVar.update { pieces =>
      pieces + (location -> piece)
    }

    if (gameState.treasuresVar.now().contains(location)) {
      gameState.playerGoldVar.update(gold => gold - piece.pieceType.value + TreasureValue)
      gameState.treasuresVar.update(treasures => treasures - location)
    } else {
      gameState.playerGoldVar.update(gold => gold - piece.pieceType.value)
    }

    gameState.connection.socket.send(write(PlacePiece(gameState.player, piece, location)))
    gameState.selectedPiece.set(None)
    gameState.moveTurnBus.emit()
  }

  // def getNextGameState(
  //   clientGameState: ClientGame,
  //   playerAction: PlayerAction
  // ): GameState = {

  //   val playerGold = clientGameState.playerGoldVar.now()
  //   val oppGold = clientGameState.opponentGoldVar.now()

  //   val gameStateRepr = GameState(
  //     clientGameState.gameId,
  //     clientGameState.player,
  //     clientGameState.opponent,
  //     clientGameState.boardVar.now(),
  //     clientGameState.piecesVar.now(),
  //     Map(clientGameState.player -> playerGold, clientGameState.opponent -> oppGold),
  //     clientGameState.treasuresVar.now(),
  //     clientGameState.play
  //   )
  // }

  def updateGameStateVariables(
    clientGameState: ClientGame,
    nextGameState: GameState
  ): Unit = {
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
