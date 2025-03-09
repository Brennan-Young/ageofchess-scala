package com.ageofchess.server

import cask.{MainRoutes, Response}
import upickle.default._
import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._

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

  def applyMove(state: collection.mutable.Map[Location, RenderablePiece], move: Move): Unit = {
    state.get(move.from).foreach { piece =>
      state.remove(move.from)
      state.update(move.to, piece)
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