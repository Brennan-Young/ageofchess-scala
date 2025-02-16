package tld.ageofchess.client

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.dom

import com.raquo.laminar.api.L._



object Main extends App {
  // import javascriptLogo from "/javascript.svg"
  @js.native @JSImport("/javascript.svg", JSImport.Default)
  val javascriptLogo: String = js.native

  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    appElement()
  )

  def appElement(): Element = {
    div(
      a(href := "https://vitejs.dev", target := "_blank",
        img(src := "/vite.svg", className := "logo", alt := "Vite logo"),
      ),
      a(href := "https://developer.mozilla.org/en-US/docs/Web/JavaScript", target := "_blank",
        img(src := javascriptLogo, className := "logo vanilla", alt := "JavaScript logo"),
      ),
      h1("Hello Laminar!"),
      div(className := "card",
        counterButton()
      ),
      p(className := "read-the-docs",
        "Click on the Vite logo to learn more",
      ),
    )
  }

  def counterButton(): Element = {
    val counter = Var(0)
    button(
      tpe := "button",
      "count is ",
      child.text <-- counter,
      onClick --> { event => counter.update(c => c + 1) },
    )
  }

  // dom.document.querySelector("#app").innerHTML = s"""
  //   <div>
  //     <a href="https://vitejs.dev" target="_blank">
  //       <img src="/vite.svg" class="logo" alt="Vite logo" />
  //     </a>
  //     <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript" target="_blank">
  //       <img src="$javascriptLogo" class="logo vanilla" alt="JavaScript logo" />
  //     </a>
  //     <h1>Hello Scala.js!</h1>
  //     <div class="card">
  //       <button id="counter" type="button"></button>
  //     </div>
  //     <p class="read-the-docs">
  //       Click on the Vite logo to learn more
  //     </p>
  //   </div>
  // """

  // setupCounter(dom.document.getElementById("counter"))

  // def setupCounter(element: dom.Element): Unit = {
  //   var counter = 0

  //   def setCounter(count: Int): Unit = {
  //     counter = count
  //     element.innerHTML = s"count is $counter"
  //   }

  //   element.addEventListener("click", {e: dom.Event => setCounter(counter + 1)})
  //   setCounter(0)
  // }

}