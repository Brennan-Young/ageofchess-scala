package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Lobby
import com.ageofchess.client.api.Sockets.LobbySocket
import com.ageofchess.client.api.Queries
import com.ageofchess.shared.Messages._
import org.scalajs.dom
import org.scalajs.dom.MessageEvent
import upickle.default.{read, write}
import com.ageofchess.client.api.Sockets.onOpenOrNow
import scala.concurrent.ExecutionContext

object LobbyPage {

  val lobbiesVar: Var[List[Lobby]] = Var(List())
  val createGameModalVisible: Var[Boolean] = Var(false)
  val createGameLoading: Var[Boolean] = Var(false)
  val createGameError: Var[Option[String]] = Var(None)

  val updateLobbiesHandler: MessageEvent => Unit = event => {
    val message: GameMessage = read[GameMessage](event.data.toString)

      message match {
        case UpdateLobbies(lobbies) => {
          println("Updating lobbies")
          lobbiesVar.set(lobbies)
        }
        case _ =>
      }
  }

  def render(connection: LobbySocket)(implicit ec: ExecutionContext): Div = {

    connection.socket.addEventListener("message", updateLobbiesHandler)

    onOpenOrNow(connection.socket) {
      connection.socket.send(write(FetchLobbies()))
    }

    div(
      h1("Available Games"),
      child <-- lobbiesVar.signal.map(lobbies => renderLobbies(lobbies)),
      button(
        cls := "create-game-button",
        "Create game",
        onClick --> (_ => {
          createGameError.set(None)
          createGameModalVisible.set(true)
        })
      ),
      renderCreateGameModal
    )
  }

  def renderCreateGameModal(implicit ec: ExecutionContext): Div = {
    div(
      cls := "create-game-overlay",
      cls.toggle("visible") <-- createGameModalVisible.signal,
      onClick --> (_ => createGameModalVisible.set(false)),
      div(
        cls := "create-game-modal",
        onClick.stopPropagation --> (_ => ()),
        h2("Create game"),
        p(cls := "create-game-placeholder", "Game settings (timers, gold, spectators) coming soon."),
        div(
          cls := "create-game-actions",
          button(
            cls := "create-game-cancel",
            "Cancel",
            onClick --> (_ => createGameModalVisible.set(false))
          ),
          button(
            cls := "create-game-submit",
            disabled <-- createGameLoading.signal,
            child.text <-- createGameLoading.signal.map(if (_) "Creating..." else "Create"),
            onClick --> { _ =>
              createGameLoading.set(true)
              createGameError.set(None)
              Queries.createGame().onComplete {
                case scala.util.Success(gameId) =>
                  createGameLoading.set(false)
                  createGameModalVisible.set(false)
                  dom.window.location.href = s"/game/${gameId}?as=player"
                case scala.util.Failure(err) =>
                  createGameLoading.set(false)
                  createGameError.set(Some(err.getMessage))
              }
            }
          )
        ),
        child <-- createGameError.signal.map {
          case Some(msg) => p(cls := "create-game-error", msg)
          case None      => emptyNode
        }
      )
    )
  }

  def renderLobbies(lobbies: List[Lobby]): Div = {
    div(
      lobbies.map { lobby =>
        div(
          cls := "lobby-entry",
          h3(s"Game ${lobby.gameId}"),
          p(s"Players: ${lobby.players.size}/2"),
          renderJoinButtons(lobby)
        )
      }
    )
  }

  def renderJoinButtons(lobby: Lobby) = {
    val canJoinAsPlayer = lobby.players.size < 2
    val canJoinAsSpectator = lobby.spectatorsAllowed

    div(
      if (canJoinAsPlayer)
        a(href := s"/game/${lobby.gameId}?as=player", "Join Game")
      else emptyNode,
      if (canJoinAsSpectator)
        a(href := s"/game/${lobby.gameId}?as=spectator", "Spectate Game")
      else emptyNode
    )
  }
}
