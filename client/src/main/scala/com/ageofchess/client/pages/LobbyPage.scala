package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Lobby
import com.ageofchess.client.api.Sockets.LobbySocket
import com.ageofchess.shared.Messages._
import org.scalajs.dom.MessageEvent
import upickle.default.{read, write}

object LobbyPage {
  val lobbiesVar: Var[List[Lobby]] = Var(List())

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

  def render(connection: LobbySocket) = {

    connection.socket.addEventListener("message", updateLobbiesHandler)

    div(
      h1("Available Games"),
      child <-- lobbiesVar.signal.map(lobbies => renderLobbies(lobbies))
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
