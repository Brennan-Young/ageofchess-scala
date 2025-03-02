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

object Board {
  def renderBoard(board: Vector[Vector[RenderableSquare]]): Node = {
    val numColumns = board.headOption.map(_.size).getOrElse(0)

    div(
      cls := "board",
      styleAttr := s"grid-template-columns: repeat(${numColumns}, 50px);",
      board.zipWithIndex.map { case (row, rIdx) =>
        div(
          cls := "board-row",
          row.zipWithIndex.map { case (square, cIdx) =>
            img(
              cls := "board-square",
              src := s"/assets/${square.asset}",
              alt := s"Square at ($rIdx, $cIdx)"
            )
          }
        )
      }
    )
  }

  def renderState(boardState: Vector[Vector[(RenderableSquare, Option[RenderablePiece])]]): HtmlElement = {
    val numColumns = boardState.headOption.map(_.size).getOrElse(0)

    div(
      cls := "board",
      styleAttr := s"grid-template-columns: repeat(${numColumns}, 50px);",
      boardState.zipWithIndex.map { case (row, rIdx) =>
        div(
          cls := "board-row",
          row.zipWithIndex.map { case (square, cIdx) =>
            renderSquare(square._1, square._2)
          }
        )  
      }
    )
  }

  def renderSquare(square: RenderableSquare, piece: Option[RenderablePiece]): HtmlElement = {
    div(
      cls := "board-square",
      backgroundImage := s"url(/assets/${square.asset})",
      piece.map(p => img(src := s"/assets/pieces/${p.asset}", cls := "piece"))
    )
  }
}

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
      case "/"      => homePage()
      case "/game"  => gamePage()
      case _        => notFoundPage()
    }

    render(dom.document.body, app)
  }

  def homePage(): Div = div(
    h1("Welcome to Age of Chess"),
    p("Click below to start a game."),
    a(href := "/game", "Start Game")
  )

  def gamePage(): Div = {
    val board: Future[Vector[Vector[RenderableSquare]]] = dom.fetch("/api/board").toFuture.flatMap { resp =>
      resp.text().toFuture
    }
      .map { json =>
        read[Board](json).toRenderable
      }

    val boardVar: Var[Option[Vector[Vector[RenderableSquare]]]] = Var(None)

    board.foreach { renderableBoard => boardVar.set(Some(renderableBoard)) }

    val piecesVar: Var[Option[Map[Location, RenderablePiece]]] = Var(None)

    piecesVar.set(Some(defaultPieces))

    val boardStateSignal: Signal[Option[Vector[Vector[(RenderableSquare, Option[RenderablePiece])]]]] = Signal.combine(boardVar.signal, piecesVar.signal).map {
      case (Some(board), Some(pieces)) => {
        val zippedBoard = board.zipWithIndex.map { case (row, rIdx) =>
          row.zipWithIndex.map { case (square, cIdx) =>
            val pieceOpt = pieces.get(Location(rIdx, cIdx))
            (square, pieceOpt)
          }  
        }
        Some(zippedBoard)
      }
      case _ => None
    }

    div(
      h1("Game Board"),
      // child <-- boardVar.signal.map {
      //   case Some(b) => Board.renderBoard(b)
      //   case None => div("Loading")
      // },
      child <-- boardStateSignal.map {
        case Some(b) => Board.renderState(b)
        case None => div("Loading")
      },
      a(href := "/", "Back to Home")
    )
  }

  def notFoundPage(): Div = div(
    h1("404 - Page Not Found"),
    p("Oops! This page does not exist."),
    a(href := "/", "Go Back Home")
  )
}
