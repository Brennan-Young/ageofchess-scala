package com.ageofchess.client.pages

import com.raquo.laminar.api.L._

object HomePage {
  def render: Div = div(
      h1("Welcome to Age of Chess"),
      a(href := "/lobbies", "Lobbies")
    )
}
