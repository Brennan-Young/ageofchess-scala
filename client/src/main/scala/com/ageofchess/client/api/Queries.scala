package com.ageofchess.client.api

import org.scalajs.dom
import upickle.default._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import com.ageofchess.shared.board.{Board, RenderableSquare}

object Queries {
  def fetchBoard()(implicit ec: ExecutionContext): Future[Vector[Vector[RenderableSquare]]] = {
    dom.fetch("/api/board").toFuture.flatMap { resp =>
      resp.text().toFuture
    }
      .map { json =>
        read[Board](json).toRenderable
      }
  }
}
