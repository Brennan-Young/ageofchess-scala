package com.ageofchess.client.api

import org.scalajs.dom
import com.ageofchess.shared.piece._
import upickle.default._

object Sockets {
  val ws = if (dom.window.location.protocol == "https:") {
    s"wss://${dom.window.location.host}/gameState"
  } else {
    s"ws://${dom.window.location.host}/gameState"
  }

  val gameStateSocket = new dom.WebSocket(ws)

  def sendMove(move: Move): Unit = {
    gameStateSocket.send(write(move))
  }
}
