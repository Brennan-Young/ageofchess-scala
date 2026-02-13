package com.ageofchess.client.pages

import com.raquo.laminar.api.L._
import com.ageofchess.shared.Lobby
import com.ageofchess.shared.game.GameSettings
import com.ageofchess.client.api.Sockets.LobbySocket
import com.ageofchess.client.api.Queries
import com.ageofchess.shared.Messages._
import org.scalajs.dom
import org.scalajs.dom.MessageEvent
import upickle.default.{read, write}
import com.ageofchess.client.api.Sockets.onOpenOrNow
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object LobbyPage {

  /** Allowed starting timer values: 15s, 30s, 45s, 1m–20m (by minute), 20m–60m (by 5 min). */
  val allowedDurationsSeconds: List[Int] = {
    val seconds = List(15, 30, 45)
    val oneTo20Minutes = (1 to 20).map(_ * 60).toList
    val twentyTo60By5 = (25 to 60 by 5).map(_ * 60).toList
    seconds ++ oneTo20Minutes ++ twentyTo60By5
  }

  val lobbiesVar: Var[List[Lobby]] = Var(List())
  val createGameModalVisible: Var[Boolean] = Var(false)
  val createGameLoading: Var[Boolean] = Var(false)
  val createGameError: Var[Option[String]] = Var(None)
  val gameSettingsVar: Var[GameSettings] = Var(GameSettings.default)

  def clockIndex(settings: GameSettings): Int =
    (allowedDurationsSeconds.indexOf(settings.initialClock.toSeconds.toInt) max 0).min(allowedDurationsSeconds.length - 1)

  def formatDuration(seconds: Int): String =
    if (seconds < 60) s"${seconds}s"
    else if (seconds % 60 == 0) s"${seconds / 60} min"
    else s"${seconds / 60}m ${seconds % 60}s"

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

  val createGameBus = new EventBus[dom.MouseEvent]

  def renderCreateGameModal(implicit ec: ExecutionContext): Div = {
    div(
      cls := "create-game-overlay",
      cls.toggle("visible") <-- createGameModalVisible.signal,
      onClick --> (_ => createGameModalVisible.set(false)),
      div(
        cls := "create-game-modal",
        onClick.stopPropagation --> (_ => ()),
        h2("Create game"),
        div(
          cls := "create-game-timer-row",
          label("Starting timer: "),
          child.text <-- gameSettingsVar.signal.map(s => formatDuration(s.initialClock.toSeconds.toInt)),
          input(
            cls := "create-game-timer-slider",
            typ := "range",
            value <-- gameSettingsVar.signal.map(s => clockIndex(s).toString),
            onInput.mapToValue --> (value => gameSettingsVar.update(_.copy(initialClock = allowedDurationsSeconds(value.toInt).seconds))),
            onMountCallback { ctx =>
              ctx.thisNode.ref.setAttribute("min", "0")
              ctx.thisNode.ref.setAttribute("max", (allowedDurationsSeconds.length - 1).toString)
            }
          )
        ),
        div(
          cls := "create-game-map-size-row",
          label("Map size: "),
          child.text <-- gameSettingsVar.signal.map(s => s"${s.boardSize}×${s.boardSize}"),
          input(
            cls := "create-game-map-size-slider",
            typ := "range",
            value <-- gameSettingsVar.signal.map(_.boardSize.toString),
            onInput.mapToValue --> (value => {
              val n = value.toInt
              gameSettingsVar.update(_.copy(boardSize = (n max GameSettings.MinBoardSize) min GameSettings.MaxBoardSize))
            }),
            onMountCallback { ctx =>
              ctx.thisNode.ref.setAttribute("min", GameSettings.MinBoardSize.toString)
              ctx.thisNode.ref.setAttribute("max", GameSettings.MaxBoardSize.toString)
            }
          )
        ),
        p(cls := "create-game-placeholder", "Other settings (gold, spectators) coming soon."),
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
            onClick --> createGameBus.writer,
            createGameBus.events.withCurrentValueOf(gameSettingsVar.signal) --> Observer[(dom.MouseEvent, GameSettings)] { case (_, settings) =>
              createGameLoading.set(true)
                createGameError.set(None)
                Queries.createGame(settings).onComplete {
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
