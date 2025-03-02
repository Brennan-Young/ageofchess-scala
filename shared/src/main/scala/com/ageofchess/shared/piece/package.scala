package com.ageofchess.shared

package object piece {
  sealed trait Color { def id: String }
  case object Black extends Color { override def id: String = "b" }
  case object White extends Color { override def id: String = "w" }

  trait PieceType { def id: String }
  case object Pawn extends PieceType { override def id: String = "pawn" }
  case object Knight extends PieceType { override def id: String = "knight" }
  case object Bishop extends PieceType { override def id: String = "bishop" }
  case object Rook extends PieceType { override def id: String = "rook" }
  case object Queen extends PieceType { override def id: String = "queen" }
  case object King extends PieceType { override def id: String = "king" }

  case class RenderablePiece(color: Color, pieceType: PieceType) {
    def asset: String = s"${color.id}_${pieceType.id}.png"
  }

  case class Location(x: Int, y: Int)

  val defaultPieces: Map[Location, RenderablePiece] = Map(
    Location(0, 0) -> RenderablePiece(White, King),
    Location(1, 3) -> RenderablePiece(Black, King)
  )
}
