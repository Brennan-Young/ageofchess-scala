package com.ageofchess.shared

import upickle.default._

package object piece {
  sealed trait Color { def id: String }

  object Color {
    implicit val rw: ReadWriter[Color] = readwriter[String].bimap[Color](
      {
        case Black => "black"
        case White => "white"
      },
      {
        case "black" => Black
        case "white" => White
      }
    )
  }

  case object Black extends Color { override def id: String = "b" }
  case object White extends Color { override def id: String = "w" }

  trait PieceType { def id: String }

  object PieceType {
    implicit val rw: ReadWriter[PieceType] = readwriter[String].bimap[PieceType](
      {
        case Pawn => "pawn"
        case Knight => "knight"
        case Bishop => "bishop"
        case Rook => "rook"
        case Queen => "queen"
        case King => "king"
      },
      {
        case "pawn" => Pawn
        case "knight" => Knight
        case "bishop" => Bishop
        case "rook" => Rook
        case "queen" => Queen
        case "king" => King
      }
    )
  }
  case object Pawn extends PieceType { override def id: String = "pawn" }
  case object Knight extends PieceType { override def id: String = "knight" }
  case object Bishop extends PieceType { override def id: String = "bishop" }
  case object Rook extends PieceType { override def id: String = "rook" }
  case object Queen extends PieceType { override def id: String = "queen" }
  case object King extends PieceType { override def id: String = "king" }

  case class RenderablePiece(color: Color, pieceType: PieceType) {
    def asset: String = s"${color.id}_${pieceType.id}.png"
  }

  object RenderablePiece {
    implicit val rw: ReadWriter[RenderablePiece] = macroRW
  }

  case class Location(x: Int, y: Int)
  
  object Location {
    implicit val rw: ReadWriter[Location] = macroRW
  }

  // case class Move(from: Location, to: Location)

  // object Move {
  //   implicit val rw: ReadWriter[Move] = macroRW
  // }

  val defaultPieces: Map[Location, RenderablePiece] = Map(
    Location(0, 0) -> RenderablePiece(White, King),
    Location(1, 3) -> RenderablePiece(Black, King)
  )
}
