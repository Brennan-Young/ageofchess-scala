package com.ageofchess.client.gamestate

import com.raquo.laminar.api.L._
import com.ageofchess.client.api.Sockets.GameSocket
import scala.concurrent.duration._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._
import com.ageofchess.shared.user.Player
import org.scalajs.dom.MessageEvent
import upickle.default.read
import com.ageofchess.shared.Messages._
import com.ageofchess.shared.game._
import com.ageofchess.client.board.AnimatingMove

class SpectatorGameView(
  val gameId: String,
  val white: Player,
  val black: Player,
  val connection: GameSocket
) {

  val piecesVar: Var[Map[Location, Piece]] = Var(Map())
  val moveAnimationVar: Var[Option[AnimatingMove]] = Var(None)
  val boardVar: Var[Option[Vector[Vector[SquareType]]]] = Var(None)
  val treasuresVar: Var[Set[Location]] = Var(Set())

  val whiteGoldVar: Var[Int] = Var(0)
  val blackGoldVar: Var[Int] = Var(0)

  val whiteClockVar = Var(0.minutes)
  val blackClockVar = Var(0.minutes)

  val gameResultVar: Var[GameResult] = Var(Unresolved)

  val playerToMoveVar = Var(white)

  val isWhiteToMove = playerToMoveVar.signal.map(_ == white)
  val isBlackToMove = playerToMoveVar.signal.map(_ == black)

  val isGameResolvedSignal = gameResultVar.signal.map(_ != Unresolved)

  connection.socket.addEventListener("message", { event: MessageEvent => 
    val message = read[GameMessage](event.data.toString)

    message match {
      case InitializeBoard(board, pieces, treasures, gold) => {
        boardVar.set(Some(board.squares))
        piecesVar.set(pieces)
        treasuresVar.set(treasures)
        gold.get(white).foreach(whiteGoldVar.set(_))
        gold.get(black).foreach(blackGoldVar.set(_))
      }
      case UpdatePlayerClocks(clocks) => {
        clocks.get(white).foreach(clock => whiteClockVar.set(clock.remaining))
        clocks.get(black).foreach(clock => blackClockVar.set(clock.remaining))
      }
      case UpdateBoardState(nextActivePlayer, playerAction, pieces, gold, treasures) => {
        val moveAnimation = playerAction match {
          case PieceMove(piece, from, to) => Some(AnimatingMove(piece, from, to))
          case PiecePlacement(_, _) => None
        }
        moveAnimationVar.set(moveAnimation)
        piecesVar.set(pieces)
        treasuresVar.set(treasures)
        gold.get(white).foreach(whiteGoldVar.set(_))
        gold.get(black).foreach(blackGoldVar.set(_))
        playerToMoveVar.set(nextActivePlayer)
      }
      case ResolveGame(result) => {
        gameResultVar.set(result)
      }
      case _ =>
    }
  })
}
