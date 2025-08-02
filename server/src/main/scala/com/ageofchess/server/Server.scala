package com.ageofchess.server

import cask.{MainRoutes, Response}
import upickle.default._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.game._
import com.ageofchess.shared.Messages._
import collection.mutable
import com.ageofchess.shared.board.BoardGenerator
import scala.concurrent.duration._
import java.time.Instant
import java.time.Duration

object Server extends MainRoutes {
  @cask.staticFiles("/js/")
  def serve() = "client/target/scala-2.12/client-fastopt"

  @cask.staticFiles("assets")
  def serveAssets() = "client/src/main/resources/assets"

  @cask.staticFiles("public")
  def serveCss() = "client/src/main/resources/public"

  @cask.get("/")
  def serveHomePage(vscodeBrowserReqId: Option[String] = None): Response[String] = {
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

  @cask.get("/game/:gameId")
  def serveGamePage(gameId: String, vscodeBrowserReqId: Option[String] = None): Response[String] = {
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

  val games = collection.mutable.Map.empty[String, ActiveGame]
  val pendingGames = collection.mutable.Map.empty[String, PendingGame]
  val playerChannels = collection.mutable.Map.empty[String, cask.WsChannelActor]

  @cask.websocket("/game/:gameId")
  def gameSocket(gameId: String): cask.WebsocketResult = {
    cask.WsHandler { channel =>
      games.get(gameId) match {
        case Some(game) => // TODO: send some sort of "game full" message
        case None => {
          pendingGames.get(gameId) match {
            case Some(game) => matchGame(game, channel)
            case None => createGame(gameId, channel)
          }
        }
      }

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
        // TODO: extract this out
        playerChannels.get(game.gameState.white.id).foreach { connection =>
          val assignments = AssignPlayers(game.gameState.white, game.gameState.black)
          connection.send(cask.Ws.Text(write(assignments)))
        }
        playerChannels.get(game.gameState.black.id).foreach { connection =>
          val assignments = AssignPlayers(game.gameState.black, game.gameState.white)
          connection.send(cask.Ws.Text(write(assignments)))  
        }
      }
      case AwaitingBoard(playerId) => {
        val serverMessage = InitializeBoard(
          game.gameState.board,
          game.gameState.pieces.toMap,
          game.gameState.treasures.toSet
        )
        playerChannels.get(playerId).foreach { connection =>
          connection.send(cask.Ws.Text(write(serverMessage)))
        }
      }
      case playerAction: PlayerActionMessage => {
        val nextGameState = game.gameState.validateAndGenerateNextState(playerAction.player, playerAction.toPlayerAction)

        val nextClocks = game.clocks.get(playerAction.player).map { clock =>
          val time = Instant.now().toEpochMilli()
          val elapsed = time - clock.lastUpdate

          val nextRemainder = clock.remaining - elapsed.millis
          val nextClock = PlayerClock(nextRemainder, time)

          game.clocks.updated(playerAction.player, nextClock)
        }

        for {
          state <- nextGameState
          clock <- nextClocks
        } {

          val nextGame = game.copy(gameState = state, clocks = clock)

          games.update(gameId, nextGame)
          val serverMessage = UpdateBoardState(state.playerToMove, state.pieces, state.gold, state.treasures)
          broadcastToPlayers(state, write(serverMessage))

          val clockMessage = UpdatePlayerClocks(clock)

          broadcastToPlayers(state, write(clockMessage))
        }

        // nextGameState.foreach { nextState =>

        //   val nextGame = game.copy(gameState = nextState)

        //   games.update(gameId, nextGame)
        //   val serverMessage = UpdateBoardState(nextState.playerToMove, nextState.pieces, nextState.gold, nextState.treasures)
        //   broadcastToPlayers(nextState, write(serverMessage))
        // }
      }
    }
  }

  def broadcastToPlayers(game: GameState, message: String) = {
    playerChannels.get(game.white.id).foreach { connection =>
      connection.send(cask.Ws.Text(message))
    }

    playerChannels.get(game.black.id).foreach { connection =>
      connection.send(cask.Ws.Text(message))  
    }
  }
 
  def createGame(gameId: String, channel: cask.WsChannelActor): Unit = {
    val playerId = java.util.UUID.randomUUID().toString
    val pending = PendingGame(gameId, playerId)
    pendingGames.update(gameId, pending)
    playerChannels.update(playerId, channel)

    println(s"Player connected : $playerId")
  }

  import scala.util.Random
  def matchGame(pendingGame: PendingGame, channel: cask.WsChannelActor): Unit = {
    val playerId = java.util.UUID.randomUUID().toString
    val coin = Random.nextInt(2)

    println(s"Second player connected: $playerId")

    val (p1, p2) = if (coin == 1) (pendingGame.player1, playerId) else (playerId, pendingGame.player1)
    val player1 = Player(p1, White)
    val player2 = Player(p2, Black)
    val board = BoardGenerator.generateBoard(20)
    val pieces = mutable.Map(defaultPieces.toSeq: _*)
    val gold = mutable.Map(player1 -> 100, player2 -> 100)
    val treasures = mutable.Set(Location(0, 1))

    val gameState = GameState(
      pendingGame.gameId,
      player1,
      player2,
      board,
      pieces.toMap,
      gold.toMap,
      treasures.toSet,
      player1
    )

    val activeGame = ActiveGame(
      gameState,
      Map(player1 -> PlayerClock(1.minute, Instant.now().toEpochMilli), player2 -> PlayerClock(1.minute, Instant.now().toEpochMilli))
    )

    games.update(pendingGame.gameId, activeGame)
    playerChannels.update(playerId, channel)
    pendingGames.remove(pendingGame.gameId)
  }
  
  initialize()
}
