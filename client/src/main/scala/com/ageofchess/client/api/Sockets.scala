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
  
  val ws2 = if (dom.window.location.protocol == "https:") {
    s"wss://${dom.window.location.host}/gameState2"
  } else {
    s"ws://${dom.window.location.host}/gameState2"
  }

  val gameState2Socket = new dom.WebSocket(ws2)

  def sendMove(move: Move): Unit = {
    gameStateSocket.send(write(move))
  }

  class GameSocket(
    gameId: String
  ) {

    val ws = if (dom.window.location.protocol == "https:") {
      s"wss://${dom.window.location.host}/game/${gameId}"
    } else {
      s"ws://${dom.window.location.host}/game/${gameId}"
    }

    val socket = new dom.WebSocket(ws)

    def sendMove(move: Move): Unit = {
      socket.send(write(move))
    }
  }

  def buildSocket(gameId: String) = {
    val ws = if (dom.window.location.protocol == "https:") {
      s"wss://${dom.window.location.host}/game/${gameId}"
    } else {
      s"ws://${dom.window.location.host}/game/${gameId}"
    }

    new dom.WebSocket(ws)
  }
}
