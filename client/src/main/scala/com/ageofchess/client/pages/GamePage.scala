package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.client.board.GameStateRenderer
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
import com.ageofchess.client.api.Sockets.GameSocket

class GamePage(val gameId: String, val pendingGame: PendingClientGame) {
  def render(implicit ec: ExecutionContext): Div = {
    div(
      h1("Game Board"),
      child <-- pendingGame.initializedPlayersSignal.signal.flatMap {
        case Some((player, opponent, startingPlayer)) => {
          // pendingGame.connection.socket.close()
          val clientGame = new ClientGame(gameId, player, opponent, startingPlayer, pendingGame.connection)
          clientGame.connection.socket.send(write(AwaitingBoard(player.id)))
          
          clientGame.boardVar.signal.map {
            case Some(board) => {
              div(
                h1(s"You are playing as ${player.color.toString}"),
                new GameStateRenderer(clientGame).render(board)
              )
            }
            case _ => div("Loading")
          }
        }
        case _ => Signal.fromValue(div("Loading"))
      },
      a(href := "/", "Back to Home")
    )
  }
}
