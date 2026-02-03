package com.ageofchess.client.board

import com.raquo.laminar.api.L._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.client.gamestate.PlayerGameView
import org.scalajs.dom
import scala.util.Try
import com.ageofchess.client.events.GameEvents._
import com.ageofchess.client.board.Clock.clockDisplay
import com.ageofchess.shared.game._

class GameStateRenderer(val clientGame: PlayerGameView) {
  val windowSize = Var((dom.window.innerWidth, dom.window.innerHeight))
  dom.window.onresize = _ => windowSize.set((dom.window.innerWidth, dom.window.innerHeight))

  def render(
    board: Vector[Vector[SquareType]]
  ): HtmlElement = {
    val numColumns = board.headOption.map(_.size).getOrElse(0)

    val squareSizeSignal = windowSize.signal.map { case (width, height) =>
      val maxWidth = width * 0.95
      val maxHeight = height * 0.95

      val squareMaxWidth = maxWidth / (numColumns + 4)
      val squareMaxHeight = maxHeight / numColumns

      math.min(squareMaxHeight, squareMaxWidth).toInt.max(20).min(80)
    }

    div(
      cls := "game-container",
      div(
        cls := "board-wrapper",
        div(
          cls := "board",
          styleAttr <-- squareSizeSignal.map { size =>
            s"grid-template-columns: repeat(${numColumns}, ${size}px);"
          },
          board.zipWithIndex.map { case (row, rIdx) =>
            div(
              cls := "board-row",
              row.zipWithIndex.map { case (square, cIdx) =>
                val squareColor = if ((rIdx + cIdx) % 2 == 0) Grass else Dirt
                val renderableSquare = RenderableSquare(squareColor, square)
                val location = Location(rIdx, cIdx)
                // val effectivePieceSignal = Signal.combine(
                //   clientGame.piecesVar.signal.map(pieces => pieces.get(location)),
                //   clientGame.animatingMovesVar.signal
                // ).map { case (pieceOpt, animating) =>
                //   if (animating.exists(_.to == location)) None else pieceOpt
                // }
                val effectivePieceSignal = Signal.combine(
                  clientGame.piecesVar.signal.map(pieces => pieces.get(location)),
                  clientGame.moveAnimationVar.signal
                ).map { case (pieceOpt, animating) =>
                  if (animating.exists(_.to == location)) None else pieceOpt
                }
                renderSquare(
                  location,
                  renderableSquare,
                  effectivePieceSignal,
                  clientGame.treasuresVar.signal.map(treasures => treasures.contains(location)),
                  clientGame.validMovesSignal.map(validMoves => validMoves.contains(location)),
                  squareSizeSignal
                )
              }
            )
          },
          onClick.map { event => findSquare(event.target).map(event -> _) } --> mouseClickBus.writer,
          mouseClickEvents(mouseClickBus, clientGame) --> mouseClickEffects(clientGame)
        ),
        renderPieceOverlay(squareSizeSignal)
      ),
      div(
        cls := "right-sidebar",
        div(
          cls := "piece-tray",
          (clientGame.player.color match {
            case White => whiteBuyablePieces
            case Black => blackBuyablePieces
          }).map { piece =>
            img(
              src := s"/assets/pieces/${piece.asset}",
              cls := "tray-piece",
              styleAttr <-- squareSizeSignal.map { size =>
                s"width: ${size}px; height: ${size}px;"
              },
              draggable := true,
              onDragStart.map(e => (e, None, piece)) --> mouseDragStartBus.writer,
              mouseDragStartEvents(mouseDragStartBus, clientGame) --> mouseDragStartEffects(clientGame)
            )
          }
        ),
        div(
          cls := "player-gold",
          child.text <-- clientGame.playerGoldSignal.map(gold => s"Your Gold: ${gold}")
        ),
        clockDisplay(clientGame.playerClockVar, clientGame.isPlayerTurnSignal, clientGame.isGameResolvedSignal),
        div(
          cls := "opponent-gold",
          child.text <-- clientGame.opponentGoldVar.signal.map(gold => s"Opponent's Gold: ${gold}")
        ),
        clockDisplay(clientGame.opponentClockVar, clientGame.isPlayerTurnSignal.map(!_), clientGame.isGameResolvedSignal)
      ),
      renderGameResult
    )
  }

  def renderPieceOverlay(squareSizeSignal: Signal[Int]): HtmlElement = {
    div(
      cls := "board-piece-overlay",
      child <-- clientGame.moveAnimationVar.signal.combineWith(squareSizeSignal).map {
        case (moves, size) =>
          div(
            moves.map { move =>
              renderAnimatingPiece(move, size)
            }
          )
      }
    )
  }

  def renderAnimatingPiece(move: AnimatingMove, size: Int): HtmlElement = {
    val positionVar = Var(move.from)
    img(
      src := s"/assets/pieces/${move.piece.asset}",
      cls := "piece board-piece-overlay-piece",
      styleAttr <-- positionVar.signal.map { loc =>
        s"left: ${loc.col * size}px; top: ${loc.row * size}px; width: ${size}px; height: ${size}px; transition: left 0.2s ease-out, top 0.2s ease-out;"
      },
      onMountCallback { _ =>
        dom.window.setTimeout(() => positionVar.set(move.to), 0)
        dom.window.setTimeout(() => clientGame.animatingMovesVar.update(_.filter(_ != move)), 250)
        dom.window.setTimeout(() => clientGame.moveAnimationVar.update(_.filter(_ != move)), 250)
      }
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
    isValidMoveOfCurrentSelectionSignal: Signal[Boolean],
    squareSizeSignal: Signal[Int]
  ): HtmlElement = {

    val isValidCaptureOfCurrentSelectionSignal: Signal[Boolean] = clientGame.validCapturesSignal.map { captures =>
      captures.contains(location)  
    }

    val isValidMoveOrCapture: Signal[Boolean] = Signal.combine(
      isValidMoveOfCurrentSelectionSignal,
      isValidCaptureOfCurrentSelectionSignal
    )
      .map { case (validMove, validCapture) => validMove || validCapture }

    div(
      cls := "board-square",
      styleAttr <-- squareSizeSignal.map { size =>
        s"width: ${size}px; height: ${size}px; background-image: url(/assets/${square.asset});"
      },
      dataAttr("row") := location.row.toString,
      dataAttr("col") := location.col.toString,
      onDragOver.preventDefault --> { _ => },
      onDrop.preventDefault.mapTo(location) --> mouseDragDropBus.writer,
      mouseDragDropEvents(
        mouseDragDropBus,
        clientGame,
        isValidMoveOrCapture,
        location
      ) --> mouseDragDropEffects(clientGame),
      child.maybe <-- pieceSignal.map { piece =>
        piece.map { p => 
          img(
            src := s"/assets/pieces/${p.asset}",
            cls := "piece",
            // TODO: this doesn't work remotely as intended, need to read up on animations more
            // styleAttr <-- squareSizeSignal.map { size =>
            //   s"transform: translate(${location.row * 50}px, ${location.col}px);"
            // },
            onDragStart.map(e => (e, Some(location), p)) --> mouseDragStartBus.writer,
            mouseDragStartEvents(mouseDragStartBus, clientGame) --> mouseDragStartEffects(clientGame)
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

  def renderGameResult: HtmlElement = {
    div(
      cls := "game-over-overlay",
      cls.toggle("visible") <-- clientGame.isGameResolvedSignal,
      onClick.preventDefault --> (_ => ()),
      child <-- clientGame.gameResultVar.signal.map {
        case GameWon(winner, reason) =>
          div(
            cls := "game-over-modal",
            h2("Game Over"),
            p(s"Winner: ${winner.userId.id}"),
            p(s"Reason: ${reason.toString}")
          )
        case _ =>
          emptyNode
      }
    )
  }
}
