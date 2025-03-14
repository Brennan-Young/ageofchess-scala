package com.ageofchess.server

import cask.{MainRoutes, Response}
import upickle.default._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.game._
import collection.mutable

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

  @cask.get("/game")
  def serveGamePage(vscodeBrowserReqId: Option[String] = None): Response[String] = {
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

  @cask.get("/api/board")
  def getBoard(): ujson.Value = {
    val board = defaultBoard
    writeJs(board)
  }

  val connections = collection.mutable.ListBuffer.empty[cask.WsChannelActor]
  val gameState = collection.mutable.Map(defaultPieces.toSeq: _*)

  val players = collection.mutable.Map.empty[String, cask.WsChannelActor] 

  def applyMove(state: collection.mutable.Map[Location, RenderablePiece], move: Move): Unit = {
    state.get(move.from).foreach { piece =>
      state.remove(move.from)
      state.update(move.to, piece)
    }
  }

  val games = collection.mutable.Map.empty[String, Game]
  val pendingGames = collection.mutable.Map.empty[String, PendingGame]
  val playerChannels = collection.mutable.Map.empty[String, cask.WsChannelActor]

  @cask.websocket("/game/:gameId")
  def gameSocket(gameId: String): cask.WebsocketResult = {
    cask.WsHandler { channel =>
      games.get(gameId) match {
        case Some(game) => handleMatchedGame(game, channel)
        case None => {
          pendingGames.get(gameId) match {
            case Some(game) => matchGame(game, channel)
            case None => createGame(gameId, channel)
          }
        }
      }
    }
  }

  def createGame(gameId: String, channel: cask.WsChannelActor) = {
    val playerId = java.util.UUID.randomUUID().toString
    val pending = PendingGame(gameId, playerId)
    pendingGames.update(gameId, pending)
    playerChannels.update(playerId, channel)

    cask.WsActor {
      case _ =>
    }
  }

  import scala.util.Random
  def matchGame(pendingGame: PendingGame, channel: cask.WsChannelActor) = {
    val playerId = java.util.UUID.randomUUID().toString
    val coin = Random.nextInt(2)

    val (p1, p2) = if (coin == 1) (pendingGame.player1, playerId) else (playerId, pendingGame.player1)
    val player1 = Player(p1, White)
    val player2 = Player(p2, Black)

    val game = Game(
      pendingGame.gameId,
      player1,
      player2,
      defaultBoard,
      mutable.Map(defaultPieces.toSeq: _*),
      true
    )

    games.update(pendingGame.gameId, game)
    playerChannels.update(playerId, channel)

    cask.WsActor {
      case _ =>
    }
  }

  def handleMatchedGame(game: Game, channel: cask.WsChannelActor) = {
    cask.WsActor {
      case cask.Ws.Text(message) => // TODO: figure out what messages can be received
      // for now something simple? A message consisting of the playerId and their move
    }
  }

  @cask.websocket("/gameState2")
  def gameState2Socket(): cask.WebsocketResult = {
    cask.WsHandler { channel =>
      val playerId = java.util.UUID.randomUUID().toString
      players += (playerId -> channel)

      println(s"Player connected: $playerId")

      cask.WsActor {
        case cask.Ws.Text(msg) => println(s"Received message from $playerId: $msg")
        case cask.Ws.Close(_, _) =>
          players -= playerId
          println(s"Player disconnected: $playerId")
      }  
    }
  }

  @cask.websocket("/gameState")
  def gameStateSocket(): cask.WebsocketResult = {
    cask.WsHandler { channel =>
      connections += channel
      cask.WsActor {
        case cask.Ws.Text(message) =>
          val move = read[Move](message)
          applyMove(gameState, move)
          gameState.update(Location(0, 2), RenderablePiece(White, Knight))
          broadcastState()
      }
    }
  }

  def broadcastState(): Unit = {
    val stateJson = write(gameState)
    connections.foreach(_.send(cask.Ws.Text(stateJson)))
  }

  initialize()
}