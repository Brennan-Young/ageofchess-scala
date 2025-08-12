package com.ageofchess.client.api

import org.scalajs.dom
import com.ageofchess.shared.piece._
import upickle.default._
import com.ageofchess.shared.user.UserRole
import com.ageofchess.shared.user.UserId

object Sockets {
  // val ws = if (dom.window.location.protocol == "https:") {
  //   s"wss://${dom.window.location.host}/gameState"
  // } else {
  //   s"ws://${dom.window.location.host}/gameState"
  // }

  // val gameStateSocket = new dom.WebSocket(ws)
  
  // val ws2 = if (dom.window.location.protocol == "https:") {
  //   s"wss://${dom.window.location.host}/gameState2"
  // } else {
  //   s"ws://${dom.window.location.host}/gameState2"
  // }

  // val gameState2Socket = new dom.WebSocket(ws2)

  // def sendMove(move: Move): Unit = {
  //   gameStateSocket.send(write(move))
  // }

  class GameSocket(
    userId: UserId,
    gameId: String,
    role: UserRole
  ) {

    val ws = if (dom.window.location.protocol == "https:") {
      s"wss://${dom.window.location.host}/game/${gameId}?user=${userId.id}&as=${role.toString}"
    } else {
      s"ws://${dom.window.location.host}/game/${gameId}?user=${userId.id}&as=${role.toString}"
    }

    val socket = new dom.WebSocket(ws)
  }

  // def buildSocket(gameId: String) = {
  //   val ws = if (dom.window.location.protocol == "https:") {
  //     s"wss://${dom.window.location.host}/game/${gameId}"
  //   } else {
  //     s"ws://${dom.window.location.host}/game/${gameId}"
  //   }

  //   new dom.WebSocket(ws)
  // }

  class LobbySocket() {
    val ws = if (dom.window.location.protocol == "https:") {
      s"wss://${dom.window.location.host}/lobbies"
    } else {
      s"ws://${dom.window.location.host}/lobbies"
    }

    val socket = new dom.WebSocket(ws)
  }

  def onOpenOrNow(socket: dom.WebSocket)(action: => Unit): Unit = {
    socket.onopen = _ => action
    if (socket.readyState == dom.WebSocket.OPEN) action
  }
}
