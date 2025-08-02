package com.ageofchess.shared.game

import scala.concurrent.duration._
import java.time.Instant
import upickle.default.{ReadWriter, macroRW}

case class PlayerClock(
  remaining: FiniteDuration,
  lastUpdate: Long
)

object PlayerClock {
  implicit val rw: ReadWriter[PlayerClock] = macroRW
}
