package com.ageofchess.client.pages

import com.raquo.laminar.api.L._

object HomePage {
  def render: Div = div(
      h1("Welcome to Age of Chess"),
      p("Click below to start a game."),
      a(href := "/game", "Start Game")
    )
}
