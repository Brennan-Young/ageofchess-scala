package com.ageofchess.server

import cask.{MainRoutes, Response}
import upickle.default._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.game._
import com.ageofchess.shared.Messages._
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

  val games = collection.mutable.Map.empty[String, Game]
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
          games.get(gameId).foreach(game => handleMessage(game, msg))
        }
      }
    }
  }

  def handleMessage(game: Game, message: String): Unit = {
    println(message)
    val parsedMessage = read[ClientMessage](message)
    parsedMessage match {
      case ConnectPlayer(p) => {
        val serverMessage = InitializeBoard(game.board, game.pieces.toMap)
        broadcastToPlayers(game, write(serverMessage))
        // TODO: extract this out
        playerChannels.get(game.white.id).foreach { connection =>
          val assignments = AssignPlayers(game.white, game.black)
          connection.send(cask.Ws.Text(write(assignments)))
        }
        playerChannels.get(game.black.id).foreach { connection =>
          val assignments = AssignPlayers(game.black, game.white)
          connection.send(cask.Ws.Text(write(assignments)))  
        }
      }
      case MovePiece(from, to) => {
        game.pieces.get(from).foreach { piece =>
          game.pieces.remove(from)
          game.pieces.update(to, piece)
        }
        val serverMessage = UpdatePieces(game.pieces.toMap)
        broadcastToPlayers(game, write(serverMessage))
      }
    }
  }

  def broadcastToPlayers(game: Game, message: String) = {
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
    val board = defaultBoard2
    val pieces = mutable.Map(defaultPieces.toSeq: _*)

    val game = Game(
      pendingGame.gameId,
      player1,
      player2,
      board,
      pieces,
      true
    )

    games.update(pendingGame.gameId, game)
    playerChannels.update(playerId, channel)
    pendingGames.remove(pendingGame.gameId)
  }
  
  initialize()
}