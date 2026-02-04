package com.ageofchess.client.api

import org.scalajs.dom
import scala.scalajs.js
import upickle.default._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import com.ageofchess.shared.board.{Board, RenderableSquare}
import com.ageofchess.shared.Messages.CreateGameResponse

object Queries {
  def fetchBoard()(implicit ec: ExecutionContext): Future[Vector[Vector[RenderableSquare]]] = {
    dom.fetch("/api/board").toFuture.flatMap { resp =>
      resp.text().toFuture
    }
      .map { json =>
        read[Board](json).toRenderable
      }
  }

  def createGame()(implicit ec: ExecutionContext): Future[String] = {
    val init = new dom.RequestInit {
      method = dom.HttpMethod.POST
    }
    dom.fetch("/api/create-game", init).toFuture.flatMap { resp =>
      resp.text().toFuture
    }.map { json =>
      read[CreateGameResponse](json).gameId
    }
  }
}
