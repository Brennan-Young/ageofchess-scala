package com.ageofchess.shared

import upickle.default._
import com.ageofchess.shared.piece.Color
import scala.util.Random

package object user {
  case class UserId(id: String)

  object UserId {
    implicit val rw: ReadWriter[UserId] = macroRW

    def generate(): UserId = {
      // TODO: java.util.UUID is not valid ScalaJS.  Perhaps generate on the server and have the client query for it,
      // or generate in some other way?

      UserId(java.util.UUID.randomUUID().toString)

      // UserId(Random.nextString(12))
    }
  }

  sealed trait GameUser {
    def userId: UserId
  }

  object GameUser {
    implicit val rw: ReadWriter[GameUser] = macroRW
  }

  case class Player(userId: UserId, color: Color) extends GameUser

  object Player {
    implicit val rw: ReadWriter[Player] = macroRW
  }

  // TODO: A spectator is not an element of "pure" chess the way a player is - move this
  case class Spectator(userId: UserId) extends GameUser

  object Spectator {
    implicit val rw: ReadWriter[Spectator] = macroRW
  }

  sealed trait UserRole {
    def toString: String
  }
  case object PlayerRole extends UserRole {
    override def toString = "player"
  }
  case object SpectatorRole extends UserRole {
    override def toString = "spectator"
  }
}
