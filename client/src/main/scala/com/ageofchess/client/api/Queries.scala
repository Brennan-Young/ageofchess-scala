package com.ageofchess.client.api

import org.scalajs.dom
import scala.scalajs.js
import upickle.default._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import com.ageofchess.shared.board.{Board, RenderableSquare}
import com.ageofchess.shared.Messages.{CreateGameRequest, CreateGameResponse}
import com.ageofchess.shared.game.GameSettings

object Queries {
  def fetchBoard()(implicit ec: ExecutionContext): Future[Vector[Vector[RenderableSquare]]] = {
    dom.fetch("/api/board").toFuture.flatMap { resp =>
      resp.text().toFuture
    }
      .map { json =>
        read[Board](json).toRenderable
      }
  }

  def createGame(settings: GameSettings)(implicit ec: ExecutionContext): Future[String] = {
    val requestBody = write(CreateGameRequest(settings.initialClock.toSeconds.toInt, settings.boardSize))
    val init = new dom.RequestInit {
      method = dom.HttpMethod.POST
      body = requestBody
      headers = js.Dictionary("Content-Type" -> "application/json")
    }
    dom.fetch("/api/create-game", init).toFuture.flatMap { resp =>
      resp.text().toFuture
    }.map { json =>
      read[CreateGameResponse](json).gameId
    }
  }
}
