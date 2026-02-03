package com.ageofchess.client.board

import com.ageofchess.client.gamestate.SpectatorGameView
import org.scalajs.dom
import com.raquo.laminar.api.L._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.client.board.Clock.clockDisplay
import com.ageofchess.shared.game._

class SpectatorGameRenderer(val spectatorView: SpectatorGameView) {
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
                val effectivePieceSignal = Signal.combine(
                  spectatorView.piecesVar.signal.map(pieces => pieces.get(location)),
                  spectatorView.moveAnimationVar.signal
                ).map { case (pieceOpt, animating) =>
                  if (animating.exists(_.to == location)) None else pieceOpt
                }
                renderSquare(
                  location,
                  renderableSquare,
                  effectivePieceSignal,
                  spectatorView.treasuresVar.signal.map(treasures => treasures.contains(location)),
                  squareSizeSignal
                )
              }
            )  
          }
        ),
        renderPieceOverlay(squareSizeSignal)
      ),
      div(
        cls := "right-sidebar",
        div(
          cls := "white-gold",
          child.text <-- spectatorView.whiteGoldVar.signal.map(gold => s"White Gold: ${gold}")
        ),
        clockDisplay(spectatorView.whiteClockVar, spectatorView.isWhiteToMove, spectatorView.isGameResolvedSignal),
        div(
          cls := "black-gold",
          child.text <-- spectatorView.blackGoldVar.signal.map(gold => s"Black Gold: ${gold}")
        ),
        clockDisplay(spectatorView.blackClockVar, spectatorView.isBlackToMove, spectatorView.isGameResolvedSignal)
      ),
      renderGameResult
    )
  }

  def renderPieceOverlay(squareSizeSignal: Signal[Int]): HtmlElement = {
    div(
      cls := "board-piece-overlay",
      child <-- spectatorView.moveAnimationVar.signal.combineWith(squareSizeSignal).map {
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
        dom.window.setTimeout(() => spectatorView.moveAnimationVar.update(_.filter(_ != move)), 250)
      }
    )
  }

  def renderSquare(
    location: Location,
    square: RenderableSquare,
    pieceSignal: Signal[Option[Piece]],
    locationHasTreasureSignal: Signal[Boolean],
    squareSizeSignal: Signal[Int]
  ): HtmlElement = {

    div(
      cls := "board-square",
      styleAttr <-- squareSizeSignal.map { size =>
        s"width: ${size}px; height: ${size}px; background-image: url(/assets/${square.asset});"
      },
      dataAttr("row") := location.row.toString,
      dataAttr("col") := location.col.toString,
      child.maybe <-- pieceSignal.map { piece =>
        piece.map { p => 
          img(
            src := s"/assets/pieces/${p.asset}",
            cls := "piece"
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
      }
    )
  }

  def renderGameResult: HtmlElement = {
    div(
      cls := "game-over-overlay",
      cls.toggle("visible") <-- spectatorView.isGameResolvedSignal,
      onClick.preventDefault --> (_ => ()),
      child <-- spectatorView.gameResultVar.signal.map {
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
