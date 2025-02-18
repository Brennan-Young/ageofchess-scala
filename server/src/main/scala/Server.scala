package tld.ageofchess.server

import cask.MainRoutes
import upickle.default.{ReadWriter, macroRW}

object Server extends MainRoutes {
  println("hello world")

  @cask.get("/")
  def home(): String = "Cask is running"

  @cask.get("/hello/:name")
  def sayHello(name: String): ujson.Value = {
    ujson.Obj("message" -> s"Hello, $name")
  }

  initialize()
}