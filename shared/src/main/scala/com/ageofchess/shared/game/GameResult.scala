package com.ageofchess.shared.game

import com.ageofchess.shared.user.Player
import upickle.default._

sealed trait VictoryCondition

object VictoryCondition {
  implicit val rw: ReadWriter[VictoryCondition] = readwriter[String].bimap[VictoryCondition](
    {
      case KingCapture => "kingcapture"
      case Timeout => "timeout"
      case Resignation => "resignation"
      case GoldThreshold => "goldthreshold"
    },
    {
      case "kingcapture" => KingCapture
      case "timeout" => Timeout
      case "resignation" => Resignation
      case "goldthreshold" => GoldThreshold
    }
  )
}

case object KingCapture extends VictoryCondition
case object Timeout extends VictoryCondition
case object Resignation extends VictoryCondition
case object GoldThreshold extends VictoryCondition

sealed trait GameResult

object GameResult {
  implicit val rw: ReadWriter[GameResult] = readwriter[ujson.Value].bimap[GameResult](
    {
      case Unresolved => ujson.Str("unresolved")
      case Draw => ujson.Str("draw")
      case g: GameWon => writeJs(g)(GameWon.rw)
    },
    {
      case ujson.Str("unresolved") => Unresolved
      case ujson.Str("draw") => Draw
      case jsObj => read[GameWon](jsObj)(GameWon.rw)
    }
  )
}

case object Unresolved extends GameResult
case object Draw extends GameResult
case class GameWon(winner: Player, reason: VictoryCondition) extends GameResult
object GameWon {
  implicit val rw: ReadWriter[GameWon] = macroRW
}
