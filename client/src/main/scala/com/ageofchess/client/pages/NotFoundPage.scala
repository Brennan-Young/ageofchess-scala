package com.ageofchess.client.pages

import com.raquo.laminar.api.L._

object NotFoundPage {
  def render: Div = {
    div(
      h1("404 - Page Not Found"),
      p("Oops! This page does not exist."),
      a(href := "/", "Go Back Home")
    )
  }
}
