package com.ageofchess.client

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSON

import org.scalajs.dom

import com.raquo.laminar.api.L._

import com.ageofchess.shared.board._
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
      // resp.json().toFuture
      resp.text().toFuture
    }
      .map { json =>
        read[Board](json).toRenderable
      }

    val boardVar: Var[Option[Vector[Vector[RenderableSquare]]]] = Var(None)

    board.foreach { renderableBoard => println(renderableBoard); boardVar.set({println(renderableBoard); Some(renderableBoard)}) }

    div(
      h1("Game Board"),
      child <-- boardVar.signal.map { //b => Seq(Board.renderBoard(b.get))
        case Some(b) => Board.renderBoard(b)
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

// object Main {
//   def main(args: Array[String]): Unit = {
//     renderOnDomContentLoaded(
//       dom.document.getElementById("app"),
//       appElement()
//     )
//   }
  
//   @js.native @JSImport("/javascript.svg", JSImport.Default)
//   val javascriptLogo: String = js.native

//   def appElement(): Element = {
//     div(
//       a(href := "https://vitejs.dev", target := "_blank",
//         img(src := "/vite.svg", className := "logo", alt := "Vite logo"),
//       ),
//       a(href := "https://developer.mozilla.org/en-US/docs/Web/JavaScript", target := "_blank",
//         img(src := javascriptLogo, className := "logo vanilla", alt := "JavaScript logo"),
//       ),
//       h1("Hello Laminar!"),
//       div(className := "card",
//         counterButton()
//       ),
//       p(className := "read-the-docs",
//         "Click on the Vite logo to learn more",
//       ),
//     )
//   }

//   def counterButton(): Element = {
//     val counter = Var(0)
//     button(
//       tpe := "button",
//       "count is ",
//       child.text <-- counter,
//       onClick --> { event => counter.update(c => c + 1) },
//     )
//   }

//   // dom.document.querySelector("#app").innerHTML = s"""
//   //   <div>
//   //     <a href="https://vitejs.dev" target="_blank">
//   //       <img src="/vite.svg" class="logo" alt="Vite logo" />
//   //     </a>
//   //     <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript" target="_blank">
//   //       <img src="$javascriptLogo" class="logo vanilla" alt="JavaScript logo" />
//   //     </a>
//   //     <h1>Hello Scala.js!</h1>
//   //     <div class="card">
//   //       <button id="counter" type="button"></button>
//   //     </div>
//   //     <p class="read-the-docs">
//   //       Click on the Vite logo to learn more
//   //     </p>
//   //   </div>
//   // """

//   // setupCounter(dom.document.getElementById("counter"))

//   // def setupCounter(element: dom.Element): Unit = {
//   //   var counter = 0

//   //   def setCounter(count: Int): Unit = {
//   //     counter = count
//   //     element.innerHTML = s"count is $counter"
//   //   }

//   //   element.addEventListener("click", {e: dom.Event => setCounter(counter + 1)})
//   //   setCounter(0)
//   // }

// }