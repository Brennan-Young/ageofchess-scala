package com.ageofchess.server

import cask.{MainRoutes, Response}
import upickle.default.{ReadWriter, macroRW}

object Server extends MainRoutes {
  @cask.staticFiles("/js/")
  def serve() = "client/target/scala-2.12/client-fastopt"

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

  initialize()
}