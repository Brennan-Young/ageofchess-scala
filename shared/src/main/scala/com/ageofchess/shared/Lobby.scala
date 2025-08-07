package com.ageofchess.shared

import com.ageofchess.shared.user.Player
import upickle.default.{ReadWriter, macroRW}

final case class Lobby(
  gameId: String,
  players: List[Player],
  spectatorsAllowed: Boolean = true,
  isGameStarted: Boolean
)

object Lobby {
  implicit val rw: ReadWriter[Lobby] = macroRW
}