package com.ageofchess.shared

import com.ageofchess.shared.piece._
import upickle.default._

package object game {
  case class Player(id: String, color: Color)

  object Player {
    implicit val rw: ReadWriter[Player] = macroRW
  }
}
