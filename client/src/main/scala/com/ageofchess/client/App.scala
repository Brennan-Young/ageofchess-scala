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
import com.ageofchess.client.pages._
import com.ageofchess.client.gamestate.PendingClientGame
import com.ageofchess.client.api.Sockets.GameSocket
import com.ageofchess.client.gamestate.{UserRole, PlayerRole, SpectatorRole}
import com.ageofchess.client.gamestate.GameConnection

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
      case path if path.startsWith("/game/") => {
        parseGameRoute() match {
          case Some((gameId, role)) =>
            val connection = new GameConnection(
              gameId,
              new GameSocket(gameId),
              role
            )

            new GamePage2(gameId, connection).render

          case None => NotFoundPage.render
        }

        // val gameId = path.stripPrefix("/game/")
        // val socket = new GameSocket(gameId)
        // val gameState = new PendingClientGame(gameId, socket)
        // new GamePage(gameId, gameState).render
      }
      case _        => NotFoundPage.render
    }

    render(dom.document.body, app)
  }

  def parseGameRoute(): Option[(String, UserRole)] = {
    val url = dom.window.location.href
    val uri = new dom.URL(url)

    val maybeGameId = uri.pathname.stripPrefix("/game/") match {
      case s if s.nonEmpty => Some(s)
      case _               => None
    }

    val role = uri.searchParams.get("as") match {
      case "player"    => PlayerRole
      case "spectator" => SpectatorRole
      case _           => SpectatorRole // default fallback
    }

    maybeGameId.map(id => (id, role))
  }

  // TODO: Move out and clean up logic
  def parseGameRoute(path: String): Option[(String, UserRole)] = {
    val splitString = path.split('?')

    splitString match {
      case Array(path, role, _*) => role match {
        case "player" => Some(path, PlayerRole)
        case "spectator" => Some(path, SpectatorRole)
        case _ => None
      }
      case _ => None
    }
  }
}
