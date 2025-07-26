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

  val games = collection.mutable.Map.empty[String, GameState]
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

  def handleMessage(gameId: String, game: GameState, message: String): Unit = {
    println(message)
    val parsedMessage = read[ClientMessage](message)
    parsedMessage match {
      case ConnectPlayer(p) => {
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
      case AwaitingBoard(playerId) => {
        val serverMessage = InitializeBoard(game.board, game.pieces.toMap, game.treasures.toSet)
        playerChannels.get(playerId).foreach { connection =>
          connection.send(cask.Ws.Text(write(serverMessage)))
        }
      }
      case MovePiece(player, from, to) => {
        val action = PieceMove(from, to)
        val nextGameState = game.computeNextState(action)

        games.

        if (player == game.playerToMove) {
          game.pieces.get(from).foreach { piece =>
            game.pieces.remove(from)
            game.pieces.update(to, piece)
            if (game.treasures.contains(to)) {
              game.treasures.remove(to)
              game.gold.get(player).foreach { prevGold =>
                game.gold.update(player, prevGold + TreasureValue)
              }
            }
            game.changeActivePlayer
          }
        } else {
          // TODO no current op here, maybe a rejection message should be sent
        }
        val serverMessage = UpdateBoardState(game.playerToMove, game.pieces.toMap, game.gold.toMap, game.treasures.toSet)
        broadcastToPlayers(game, write(serverMessage))
      }
      case PlacePiece(player, piece, location) => {
        if (player == game.playerToMove) {
          game.pieces.update(location, piece)

          game.gold.get(player).foreach { playerGold =>
            val nextGold = if (game.treasures.contains(location)) {
              playerGold - piece.pieceType.value + TreasureValue
            } else {
              playerGold - piece.pieceType.value
            }

            game.gold.update(player, nextGold)
          }

          game.treasures.remove(location)

          game.changeActivePlayer
        } else {}
        val serverMessage = UpdateBoardState(game.playerToMove, game.pieces.toMap, game.gold.toMap, game.treasures.toSet)
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
    val board = defaultBoard3
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

    val game = Game(
      pendingGame.gameId,
      player1,
      player2,
      board,
      pieces,
      treasures,
      gold
    )

    games.update(pendingGame.gameId, gameState)
    playerChannels.update(playerId, channel)
    pendingGames.remove(pendingGame.gameId)
  }
  
  initialize()
}