package com.ageofchess.client

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON

import org.scalajs.dom

import com.raquo.laminar.api.L._

import com.ageofchess.shared.board._
import com.ageofchess.shared.piece._
import upickle.default._
import ujson.{read => ujsonread}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import com.ageofchess.client.board.Rendering
import com.ageofchess.client.pages._

object Main {
  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  def main(args: Array[String]): Unit = {
    val currentPath = org.scalajs.dom.window.location.pathname

    dom.document.head.appendChild {
      val link = dom.document.createElement("link")
      link.setAttribute("rel", "stylesheet")
      link.setAttribute("href", "/public/style.css")
      link
    }

    val app = currentPath match {
      case "/"      => HomePage.render
      case "/game"  => GamePage.render
      case path if path.startsWith("/game/") => {
        val gameId = path.stripPrefix("/game/")
        new GamePageClass(gameId).render
      }
      case _        => NotFoundPage.render
    }

    render(dom.document.body, app)
  }
}
