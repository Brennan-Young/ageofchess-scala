package com.ageofchess.shared

import com.ageofchess.shared.user.UserId
import upickle.default.{ReadWriter, macroRW}

final case class Lobby(
  gameId: String,
  players: List[UserId],
  spectatorsAllowed: Boolean = true,
  isGameStarted: Boolean
)

object Lobby {
  implicit val rw: ReadWriter[Lobby] = macroRW
}