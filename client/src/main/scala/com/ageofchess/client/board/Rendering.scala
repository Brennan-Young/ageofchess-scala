package com.ageofchess.client.board

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Messages._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.client.api.Sockets
import com.ageofchess.client.gamestate.PlayerGameView
import org.scalajs.dom
import scala.util.Try
import upickle.default._ // remove later
import com.ageofchess.client.events.GameEvents._
import scala.concurrent.duration._
import com.ageofchess.client.board.Clock.clockDisplay

class GameStateRenderer(val clientGame: PlayerGameView) {

  val squareSizePx = 50

  val squareSizeVar = Var(50)

  def updateSize(): Unit = {
    val windowWidth = dom.window.innerWidth
    val size = (windowWidth / 25).toInt.min(60).max(30)
    squareSizeVar.set(size)
  }

  // Attach to resize event
  dom.window.onresize = _ => updateSize()
  updateSize() // call initially

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
        cls := "board",
        styleAttr <-- squareSizeSignal.map { size =>
          s"grid-template-columns: repeat(${numColumns}, ${size}px);"
        },
        // styleAttr := s"grid-template-columns: repeat(${numColumns}, 50px);",
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
                clientGame.piecesVar.signal.map(pieces => pieces.get(location)),
                clientGame.treasuresVar.signal.map(treasures => treasures.contains(location)),
                clientGame.validMovesSignal.map(validMoves => validMoves.contains(location)),
                squareSizeSignal
              )
            }
          )
        },
        onClick.map { event => findSquare(event.target).map(event -> _) } --> mouseClickBus.writer,
        mouseClickEvents(mouseClickBus, clientGame) --> mouseClickEffects(clientGame)
        // onMountCallback { ctx =>
        //   clientGame.connection.socket.onmessage = event => {
        //     val message: GameMessage = read[GameMessage](event.data.toString)
        //     val playerToMove = clientGame.playerToMoveSignal.observe(ctx.owner)
        //     println(s"Received game message: $message")
        //     message match {
        //       case UpdateBoardState(nextActivePlayer, pieces, gold, treasures) => {
        //         clientGame.piecesVar.set(pieces)
        //         clientGame.treasuresVar.set(treasures)
        //         if (nextActivePlayer == clientGame.player && playerToMove.now() != clientGame.player) {
        //           clientGame.moveTurnBus.emit()
        //         }
        //         gold.get(clientGame.player).foreach { updatedGold =>
        //           clientGame.playerGoldVar.set(updatedGold)
        //         }
        //         gold.get(clientGame.opponent).foreach { updatedGold =>
        //           clientGame.opponentGoldVar.set(updatedGold)
        //         }
        //       }
        //       case _ => println("y")
        //     }
        //   }
        // }
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
        clockDisplay(clientGame.playerClockVar, clientGame.isPlayerTurnSignal),
        div(
          cls := "opponent-gold",
          child.text <-- clientGame.opponentGoldVar.signal.map(gold => s"Opponent's Gold: ${gold}")
        ),
        clockDisplay(clientGame.opponentClockVar, clientGame.isPlayerTurnSignal.map(!_))
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
}
