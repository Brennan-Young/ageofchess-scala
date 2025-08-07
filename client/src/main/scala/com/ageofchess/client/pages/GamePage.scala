package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.client.board.GameStateRenderer
import upickle.default._
import scala.concurrent.ExecutionContext
import com.ageofchess.shared.Messages._
import com.ageofchess.client.gamestate._
import com.ageofchess.shared.user.UserId
import com.ageofchess.shared.user.{PlayerRole, SpectatorRole}

class GamePage(val gameId: String, val pendingGame: PendingClientGame) {
  def render(implicit ec: ExecutionContext): Div = {
    div(
      h1("Game Board"),
      child <-- pendingGame.initializedPlayersSignal.signal.flatMap {
        case Some((player, opponent, startingPlayer)) => {
          // pendingGame.connection.socket.close()
          val clientGame = new PlayerGameView(gameId, player, opponent, startingPlayer, pendingGame.connection)
          pendingGame.connection.socket.removeEventListener("message", pendingGame.assignPlayers)
          clientGame.connection.socket.send(write(AwaitingBoard(player.userId.id)))
          
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

class GamePage2(val userId: UserId, val gameId: String, val gameConnection: GameConnection) {
  def render(implicit ec: ExecutionContext): Div = {
    div(
      h1("Game Board"),
      child <-- gameConnection.gameMetadataVar.signal.flatMap {
        case Some(metadata) => {
          val white = metadata.white
          val black = metadata.black

          gameConnection.connection.socket.removeEventListener("message", gameConnection.assignPlayers)

          gameConnection.role match {
            case PlayerRole => {
              val player = if (userId.id == white.userId.id) white else black
              val opponent = if (userId.id == white.userId.id) black else white
              val playerView = new PlayerGameView(gameId, player, opponent, white, gameConnection.connection)
              playerView.connection.socket.send(write(AwaitingBoard(userId.id)))
              renderPlayerView(playerView)
            }
            case SpectatorRole => {
              val spectatorView = new SpectatorGameView(gameId, white, black, gameConnection.connection)
              renderSpectatorView(spectatorView)
            }
          }
        }
        case _ => {
          Signal.fromValue(div("Awaiting players..."))
        }
      },
      a(href := "/", "Back to Home")
    )
  }

  def renderPlayerView(
    playerView: PlayerGameView
  ) = {

    playerView.boardVar.signal.map {
      case Some(board) => {
        div(
          h1(s"You are playing as ${playerView.player.color.toString}"),
          new GameStateRenderer(playerView).render(board)
        )
      }
      case None => {
        div("Loading board...")
      }
    }
  }

  def renderSpectatorView(
    spectatorView: SpectatorGameView
  ) = {

    Signal.fromValue(div("spectator view"))
  }

}

