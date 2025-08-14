package com.ageofchess.server

import cask.{MainRoutes, Response}
import upickle.default._
import com.ageofchess.shared.game._
import com.ageofchess.shared.Messages._
import scala.concurrent.duration._
import java.time.Instant
import cask.endpoints.WsChannelActor
import com.ageofchess.shared.user.UserId
import com.ageofchess.shared.game.ActiveGame
import com.ageofchess.shared.user.Spectator
import com.ageofchess.shared.Lobby

object Server extends MainRoutes {
  @cask.staticFiles("/js/")
  def serve() = "client/target/scala-2.12/client-fastopt"

  @cask.staticFiles("assets")
  def serveAssets() = "client/src/main/resources/assets"

  @cask.staticFiles("public")
  def serveCss() = "client/src/main/resources/public"

  @cask.get("/")
  def serveHomePage(): Response[String] = {
    cask.Response(
      """<!DOCTYPE html>
        <html>
        <head>
          <title>Age of Chess</title>
          <script type="module" src="/js/main.js"></script>
        </head>
        <body></body>
        </html>""",
      headers = Seq("Content-Type" -> "text/html")
    )
  }

  @cask.get("/lobbies")
  def serveLobbyPage(): Response[String] = {
    cask.Response(
      """<!DOCTYPE html>
        <html>
        <head>
          <title>Age of Chess - Lobbies</title>
          <script type="module" src="/js/main.js"></script>
        </head>
        <body></body>
      </html>""",
      headers = Seq("Content-Type" -> "text/html")
    )
  }

  @cask.get("/game/:gameId")
  def serveGamePage(gameId: String, as: Option[String] = None): Response[String] = {
    cask.Response(
      """<!DOCTYPE html>
        <html>
        <head>
          <title>Age of Chess - Game</title>
          <script type="module" src="/js/main.js"></script>
        </head>
        <body></body>
      </html>""",
      headers = Seq("Content-Type" -> "text/html")
    )
  }

  override def handleNotFound() = {
    cask.Response(
      """<!DOCTYPE html>
        <html>
        <head>
          <title>Age of Chess - 404</title>
          <script type="module" src="/js/main.js"></script>
        </head>
        <body></body>
      </html>""",
      headers = Seq("Content-Type" -> "text/html")
    )
  }

  val games = collection.mutable.Map.empty[String, ActiveGame]
  val pending = collection.mutable.Map.empty[String, Pending]
  val userChannels = collection.mutable.Map.empty[UserId, WsChannelActor]
  val lobbySubscribers = collection.mutable.Set.empty[WsChannelActor]
  var lobbies = List.empty[Lobby]

  @cask.websocket("/lobbies")
  def lobbySocket(): cask.WebsocketResult = {
    cask.WsHandler { channel =>

      lobbySubscribers += channel
      
      cask.WsActor {
        case cask.Ws.Text(msg) => {
          read[ClientMessage](msg) match {
            case FetchLobbies() => {
              updateLobbies()
            }
            case _ => {
              println(s"Lobbies socket received non-FetchLobbies message: $msg")
            }
          }
        }
        case cask.Ws.Close(_, _) => {
          lobbySubscribers -= channel
        }
      }
    }
  }

  @cask.websocket("/game/:gameId")
  def gameSocket(gameId: String, user: String, as: String): cask.WebsocketResult = {
    cask.WsHandler { channel =>
      val userId = UserId(user)
      userChannels.update(userId, channel)

      println(games.get(gameId), user, as)
      
      games.get(gameId) match {
        case None => {
          pending.get(gameId) match {
            case None => createLobby(userId, gameId, as)
            case Some(pendingLobby) => {
              as match {
                case "player" => {
                  val updatedLobby = pendingLobby.copy(players = pendingLobby.players :+ userId)
                  updatedLobby.players match {
                    case List(p1, p2, _*) => {
                      val game = constructActiveGame(gameId, p1, p2, updatedLobby.spectators)
                      pending.remove(gameId)
                      games.update(gameId, game)
                    }
                    case _ => {
                      pending.update(gameId, updatedLobby)
                    }
                  }
                }
                case "spectator" => {
                  val updatedLobby = pendingLobby.copy(spectators = pendingLobby.spectators :+ userId)
                  pending.update(gameId, updatedLobby)
                }
                case _ =>
              }
            }
          }
        }
        // case Some(game) if !game.allAssociatedClients.map(_.userId).contains(userId) => {
        case Some(game) => {
          as match {
            case "spectator" => {
              val updatedGame = game.copy(
                spectators = game.spectators :+ Spectator(userId),
              )

              games.update(gameId, updatedGame)
            }
            case "player" =>
            case _ =>
          }
        }
        // case Some(game) if game.allAssociatedClients.map(_.userId).contains(userId) => {
        //   println("Client already connected" + userId)
        // }
      }

      updateLobbies()
      
      cask.WsActor {
        case cask.Ws.Text(msg) => {
          games.get(gameId).foreach(game => handleMessage(gameId, game, msg))
        }
      }
    }
  }

  def handleMessage(gameId: String, game: ActiveGame, message: String): Unit = {
    println(message)
    val parsedMessage = read[ClientMessage](message)
    parsedMessage match {
      case ConnectPlayer(p) => {
        val message = AssignPlayers(game.gameState.white, game.gameState.black)
        broadcastToUsers(game, write(message))
      }
      case AwaitingBoard(userId) => {
        val initializedBoard = InitializeBoard(
          game.gameState.board,
          game.gameState.pieces.toMap,
          game.gameState.treasures.toSet,
          game.gameState.gold
        )

        // TODO: Unintuitive to update clocks here and can cause issues with managing time
        game.clocks.get(game.gameState.playerToMove).map { clock =>
          val time = Instant.now().toEpochMilli()
          val elapsed = time - clock.lastUpdate

          val nextRemainder = clock.remaining - elapsed.millis
          val nextClock = PlayerClock(nextRemainder, time)

          game.clocks.map { case (player, clock) =>
            player -> (if (player == game.gameState.playerToMove) nextClock else clock.copy(lastUpdate = time))
          }
        }
          .foreach { nextClock =>
            val updatedGame = game.copy(clocks = nextClock)
            games.update(gameId, updatedGame)

            val clockMessage = UpdatePlayerClocks(nextClock)

            userChannels.get(userId).foreach { connection =>
              connection.send(cask.Ws.Text(write(clockMessage)))
            }
          }

        userChannels.get(userId).foreach { connection =>
          connection.send(cask.Ws.Text(write(initializedBoard)))  
        }
      }
      case playerAction: PlayerActionMessage => {
        val nextGameState = game.gameState.validateAndGenerateNextState(playerAction.player, playerAction.toPlayerAction)

        val nextClocks = game.clocks.get(playerAction.player).map { clock =>
          val time = Instant.now().toEpochMilli()
          val elapsed = time - clock.lastUpdate

          val nextRemainder = clock.remaining - elapsed.millis
          val nextClock = PlayerClock(nextRemainder, time)

          game.clocks.map { case (player, clock) =>
            player -> (if (player == playerAction.player) nextClock else clock.copy(lastUpdate = time))
          }
        }

        for {
          state <- nextGameState
          clock <- nextClocks
        } {

          val nextGame = game.copy(gameState = state, clocks = clock)

          if (!state.containsWhiteKing) {
            val result = GameWon(state.black, KingCapture)
            broadcastToUsers(nextGame, write(ResolveGame(result)))
          } else if (!state.containsBlackKing) {
            val result = GameWon(state.white, KingCapture)
            broadcastToUsers(nextGame, write(ResolveGame(result)))
          }

          games.update(gameId, nextGame)
          val serverMessage = UpdateBoardState(state.playerToMove, state.pieces, state.gold, state.treasures)
          broadcastToUsers(nextGame, write(serverMessage))

          val clockMessage = UpdatePlayerClocks(clock)

          broadcastToUsers(nextGame, write(clockMessage))
        }
      }
    }
  }

  def broadcastToUsers(game: ActiveGame, message: String) = {
    game.allAssociatedClients.foreach { user =>
      userChannels.get(user.userId).foreach { connection =>
        connection.send(cask.Ws.Text(message))  
      }  
    }
  }

  def createLobby(user: UserId, gameId: String, role: String): Unit = {
    val pendingGame = role match {
      case "player" => Pending(gameId, List(user), List())
      case "spectator" => Pending(gameId, List(), List(user))
    }

    pending.update(gameId, pendingGame)
  }

  def updateLobbies(): Unit = {
    val activeGames = games.map { case (gameId, game) =>
      gameId -> Lobby(gameId, game.players.map(_.userId), true, true)  
    }

    val openLobbies = pending.map { case (gameId, game) =>
      gameId -> Lobby(gameId, game.players, true, false)  
    }

    val l = activeGames ++ openLobbies
    lobbies = l.map(_._2).toList

    val message = UpdateLobbies(lobbies)

    lobbySubscribers.foreach { channel =>
      channel.send(cask.Ws.Text(write(message)))
    }
  }
 
  initialize()
}
