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
import com.ageofchess.client.events.GameEvents._

class GameStateRenderer(val gameState: ClientGame) {
  def render(
    board: Vector[Vector[SquareType]],
    piecesSignal: Signal[Map[Location, Piece]],
    validMovesOfSelectionSignal: Signal[Set[Location]]
  ): HtmlElement = {
    val numColumns = board.headOption.map(_.size).getOrElse(0)

    div(
      h1(s"You are playing as ${gameState.player.color.toString}"),
      div(
        cls := "board",
        styleAttr := s"grid-template-columns: repeat(${numColumns}, 50px);",
        board.zipWithIndex.map { case (row, rIdx) =>
          div(
            cls := "board-row",
            row.zipWithIndex.map { case (square, cIdx) =>
              val squareColor = if ((rIdx + cIdx) % 2 == 0) Grass else Dirt
              val renderableSquare = RenderableSquare(squareColor, square)
              val location = Location(rIdx, cIdx)
              renderSquare(
                location,
                renderableSquare,
                piecesSignal.map(pieces => pieces.get(location)),
                gameState.treasuresVar.signal.map(treasures => treasures.contains(location)),
                validMovesOfSelectionSignal.map(validMoves => validMoves.contains(location))
              )
            }
          )
        },
        onClick.map { event => findSquare(event.target).map(event -> _) } --> mouseClickBus.writer,
        mouseClickEv(mouseClickBus, gameState) --> mouseClickEff(gameState),
        onMountCallback { ctx =>
          gameState.connection.socket.onmessage = event => {
            val message: GameMessage = read[GameMessage](event.data.toString)
            val playerToMove = gameState.playerToMoveSignal.observe(ctx.owner)
            println(s"Received game message: $message")
            message match {
              case UpdateBoardState(nextActivePlayer, pieces, gold, treasures) => {
                gameState.piecesVar.set(pieces)
                gameState.treasuresVar.set(treasures)
                if (nextActivePlayer == gameState.player && playerToMove.now() != gameState.player) {
                  gameState.moveTurnBus.emit()
                }
                gold.get(gameState.player).foreach { updatedGold =>
                  gameState.playerGoldVar.set(updatedGold)
                }
                gold.get(gameState.opponent).foreach { updatedGold =>
                  gameState.opponentGoldVar.set(updatedGold)
                }
              }
              case _ =>
            }
          }
        }
      ),
      div(
        cls := "piece-tray",
        (gameState.player.color match {
          case White => whiteBuyablePieces
          case Black => blackBuyablePieces
        }).map { piece =>
          img(
            src := s"/assets/pieces/${piece.asset}",
            cls := "tray-piece",
            draggable := true,
            onDragStart.map(e => (e, None, piece)) --> mouseDragStartBus.writer,
            mouseDragStartEvents(mouseDragStartBus, gameState) --> mouseDragStartEffects(gameState)
          )
        }
      ),
      div(
        cls := "player-gold",
        child.text <-- gameState.playerGoldSignal.map(gold => s"Your Gold: ${gold}")
      ),
      div(
        cls := "opponent-gold",
        child.text <-- gameState.opponentGoldVar.signal.map(gold => s"Opponent's Gold: ${gold}")
      )
    )
  }

  def findSquare(target: dom.EventTarget): Option[Location] = {
    target match {
      case el: dom.Element => {
        el.closest(".board-square") match {
          case null => None
          case square => for {
            row <- Try(square.getAttribute("data-row").toInt).toOption
            col <- Try(square.getAttribute("data-col").toInt).toOption
          } yield Location(row, col)
        }
      }
    }
  }

  def renderSquare(
    location: Location,
    square: RenderableSquare,
    pieceSignal: Signal[Option[Piece]],
    locationHasTreasureSignal: Signal[Boolean],
    isValidMoveOfCurrentSelectionSignal: Signal[Boolean]
  ): HtmlElement = {

    val isValidCaptureOfCurrentSelectionSignal: Signal[Boolean] = gameState.validCapturesSignal.map { captures =>
      captures.contains(location)  
    }

    val isValidMoveOrCapture: Signal[Boolean] = Signal.combine(
      isValidMoveOfCurrentSelectionSignal,
      isValidCaptureOfCurrentSelectionSignal
    )
      .map { case (validMove, validCapture) => validMove || validCapture }

    div(
      cls := "board-square",
      dataAttr("row") := location.row.toString,
      dataAttr("col") := location.col.toString,
      backgroundImage := s"url(/assets/${square.asset})",
      onDragOver.preventDefault --> { _ => },
      onDrop.preventDefault.mapTo(location) --> mouseDragDropBus.writer,
      mouseDragDropEvents(
        mouseDragDropBus,
        isValidMoveOrCapture,
        gameState.selectedPiece.signal,
        gameState.playerGoldSignal,
        location
      ) --> mouseDragDropEffects(gameState),
      child.maybe <-- pieceSignal.map { piece =>
        piece.map { p => 
          img(
            src := s"/assets/pieces/${p.asset}",
            cls := "piece",
            // TODO: this doesn't work remotely as intended, need to read up on animations more
            // styleAttr := s"transform: translate(${location.y * 50}px, ${location.x}px);",
            onDragStart.map(e => (e, Some(location), p)) --> mouseDragStartBus.writer,
            mouseDragStartEvents(mouseDragStartBus, gameState) --> mouseDragStartEffects(gameState)
          )
        }
      },
      child <-- locationHasTreasureSignal.map { hasTreasure =>
        if (hasTreasure) {
          img(
            src := s"/assets/pieces/treasure.png",
            cls := "treasure"
          )
        } else {
          emptyNode
        }
      },
      child <-- isValidMoveOfCurrentSelectionSignal.map { isValidMove =>
        if (isValidMove) {
          div(
            cls := "valid-move-marker"
          )
        } else {
          emptyNode
        }
      },
      child <-- isValidCaptureOfCurrentSelectionSignal.map { isValidCapture => 
        if (isValidCapture) {
          div(
            cls := "valid-capture-marker"
          )
        } else {
          emptyNode
        }
      }
    )
  }
}
