package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.client.board.GameStateRenderer
import com.ageofchess.client.gamestate.ClientGameState
import com.ageofchess.client.gamestate.{PendingClientGame, ClientGame}
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.game._
import scala.concurrent.Future
import upickle.default._
import scala.concurrent.ExecutionContext
import org.scalajs.dom
import com.ageofchess.client.api.Queries
import com.ageofchess.client.api.Sockets
import com.ageofchess.shared.Messages._

class GamePage(val gameId: String, val gameState: PendingClientGame) {
  val clientGameVar: Var[Option[ClientGame]] = Var(None)

  gameState.initializedBoardSignal.map {
    case Some((board, pieces, player, opponent, startingPlayer)) => {
      println("board initialized")
      val clientGame = new ClientGame(gameId, player, opponent, startingPlayer, gameState.connection)
      clientGame.boardVar.set(gameState.boardVar.now())
      clientGame.piecesVar.set(gameState.piecesVar.now())

      clientGameVar.set(Some(clientGame))
    }
    case None =>
  }

  // val boardSignal = clientGameVar.signal.flatMap {
  //   case Some(clientGame) => clientGame.boardStateSignal
  //   case None => Signal.fromValue(None)
  // }

  def render(implicit ec: ExecutionContext): Div = {
    div(
      h1("Game Board"),
      child <-- clientGameVar.signal.map {
        case (Some(clientGame)) => {
          new GameStateRenderer(clientGame).render(Vector())
        }
        case _ => div("Loading")
      },
      a(href := "/", "Back to Home")
    )
  }
}

class GamePage2(val gameId: String, val gameState: ClientGameState) {
  // def render(implicit ec: ExecutionContext): Div = {
  //   div(
  //     h1("Game Board"),
  //     child <-- gameState.boardStateSignal.map {
  //       case Some(b) => new GameStateRenderer(gameState).render(b)
  //       case None => div("Loading")
  //     },
  //     a(href := "/", "Back to Home")
  //   )
  // }
}
