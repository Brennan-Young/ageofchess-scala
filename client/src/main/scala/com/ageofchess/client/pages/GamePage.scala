package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.client.board.GameStateRenderer
import upickle.default._
import scala.concurrent.ExecutionContext
import com.ageofchess.shared.Messages._
import com.ageofchess.client.gamestate._
import com.ageofchess.shared.user.UserId
import com.ageofchess.shared.user.{PlayerRole, SpectatorRole}
import com.ageofchess.client.board.SpectatorGameRenderer

class GamePage(val userId: UserId, val gameId: String, val gameConnection: GameConnection) {
  def render(implicit ec: ExecutionContext): Div = {
    val handler = gameConnection.assignPlayers
    gameConnection.connection.socket.addEventListener("message", handler)

    div(
      h1("Game Board"),
      child <-- gameConnection.gameMetadataVar.signal.flatMap {
        case Some(metadata) => {
          val white = metadata.white
          val black = metadata.black

          gameConnection.connection.socket.removeEventListener("message", handler)

          gameConnection.role match {
            case PlayerRole => {
              val player = if (userId.id == white.userId.id) white else black
              val opponent = if (userId.id == white.userId.id) black else white
              val playerView = new PlayerGameView(gameId, player, opponent, white, gameConnection.connection)
              playerView.connection.socket.send(write(AwaitingBoard(userId)))
              renderPlayerView(playerView)
            }
            case SpectatorRole => {
              val spectatorView = new SpectatorGameView(gameId, white, black, gameConnection.connection)
              spectatorView.connection.socket.send(write(AwaitingBoard(userId)))
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

    spectatorView.boardVar.signal.map {
      case Some(board) => {
        div(
          h1("Spectator board"),
          new SpectatorGameRenderer(spectatorView).render(board)
        )
      }
      case None => {
        div("Loading board...")
      }
    }
  }
}

