package com.ageofchess.shared

import upickle.default._
import scala.collection.mutable
import scala.annotation.tailrec
import com.ageofchess.shared.board.BoardWithPieces
import com.ageofchess.shared.board.SquareType

package object piece {
  sealed trait Color { def id: String }

  object Color {
    implicit val rw: ReadWriter[Color] = readwriter[String].bimap[Color](
      {
        case Black => "black"
        case White => "white"
        case Gaia => "gaia"
      },
      {
        case "black" => Black
        case "white" => White
        case "gaia" => Gaia
      }
    )
  }

  case object Black extends Color { override def id: String = "b" }
  case object White extends Color { override def id: String = "w" }
  case object Gaia extends Color { override def id: String = "g" }

  case class MoveVector(
    x: Int,
    y: Int,
    infinite: Boolean
  )

  trait PieceType {
    def id: String

    def moves: Set[MoveVector]
    def captures: Set[MoveVector]
  }

  object PieceType {
    implicit val rw: ReadWriter[PieceType] = readwriter[String].bimap[PieceType](
      {
        case Pawn => "pawn"
        case Knight => "knight"
        case Bishop => "bishop"
        case Rook => "rook"
        case Queen => "queen"
        case King => "king"
        case Treasure => "treasure"
      },
      {
        case "pawn" => Pawn
        case "knight" => Knight
        case "bishop" => Bishop
        case "rook" => Rook
        case "queen" => Queen
        case "king" => King
        case "treasure" => Treasure
      }
    )
  }

  case object Treasure extends PieceType {
    override def id: String = "treasure"

    override def moves = Set()
    override def captures = Set()
  }

  case object Pawn extends PieceType {
    override def id: String = "pawn"

    override def moves = Set(
      MoveVector(1, 0, false),
      MoveVector(-1, 0, false),
      MoveVector(0, 1, false),
      MoveVector(0, -1, false)
    )

    override def captures = Set(
      MoveVector(1, 1, false),
      MoveVector(-1, -1, false),
      MoveVector(1, -1, false),
      MoveVector(-1, 1, false)
    )
  }

  case object Knight extends PieceType {
    override def id: String = "knight"

    override def moves = Set(
      MoveVector(2, 1, false),
      MoveVector(2, -1, false),
      MoveVector(1, 2, false),
      MoveVector(-1, 2, false),
      MoveVector(-2, 1, false),
      MoveVector(-2, -1, false),
      MoveVector(1, -2, false),
      MoveVector(-1, -2, false)
    )

    override def captures = moves
  }

  case object Bishop extends PieceType { override def id: String = "bishop"
    override def moves = Set(
      MoveVector(1, 1, true),
      MoveVector(1, -1, true),
      MoveVector(-1, -1, true),
      MoveVector(-1, 1, true)
    )

    override def captures = moves
  }

  case object Rook extends PieceType {
    override def id: String = "rook"

    override def moves = Set(
      MoveVector(1, 0, true),
      MoveVector(0, 1, true),
      MoveVector(-1, 0, true),
      MoveVector(0, -1, true)
    )

    override def captures = moves
  }

  case object Queen extends PieceType {
    override def id: String = "queen"

    override def moves = Set(
      MoveVector(1, 1, true),
      MoveVector(1, -1, true),
      MoveVector(-1, -1, true),
      MoveVector(-1, 1, true),
      MoveVector(1, 0, true),
      MoveVector(0, 1, true),
      MoveVector(-1, 0, true),
      MoveVector(0, -1, true)
    )

    override def captures = moves
  }
  case object King extends PieceType {
    override def id: String = "king"

    override def moves = Set(
      MoveVector(1, 1, false),
      MoveVector(1, -1, false),
      MoveVector(-1, -1, false),
      MoveVector(-1, 1, false),
      MoveVector(1, 0, false),
      MoveVector(0, 1, false),
      MoveVector(-1, 0, false),
      MoveVector(0, -1, false)
    )

    override def captures = moves
  }

  case class Piece(color: Color, pieceType: PieceType) {
    def asset: String = s"${color.id}_${pieceType.id}.png"
  }

  object Piece {
    implicit val rw: ReadWriter[Piece] = macroRW
  }

  case class Location(row: Int, col: Int) {
    def translate(moveVector: MoveVector) = {
      Location(row + moveVector.x, col + moveVector.y)
    }
  }
  
  object Location {
    implicit val rw: ReadWriter[Location] = macroRW
  }

  def validMoves(
    board: BoardWithPieces,
    pieceLocation: Location,
    piece: Piece
  ): Set[Location] = {

    piece.pieceType.moves.flatMap { moveVector =>
      getValidAlongVector(mutable.Set.empty[Location], board, pieceLocation, moveVector)
    }
  }

  @tailrec def getValidAlongVector(
    state: mutable.Set[Location],
    board: BoardWithPieces,
    location: Location,
    moveVector: MoveVector
  ): Set[Location] = {

    val nextSquareLocation = location.translate(moveVector)
    val nextSquare = board.getSquare(nextSquareLocation)

    nextSquare match {
      case None => state.toSet
      case Some(square) => {
        if (!square.canMoveOnto || board.pieces.contains(nextSquareLocation)) state.toSet
        else if (!square.canPassThrough) (state + nextSquareLocation).toSet
        else if (!moveVector.infinite) (state + nextSquareLocation).toSet
        else {
          getValidAlongVector(state + nextSquareLocation, board, nextSquareLocation, moveVector)
        }
      }
    }
  }

  def validCaptures(
    board: BoardWithPieces,
    pieceLocation: Location,
    piece: Piece
  ): Set[Location] = {
    
    piece.pieceType.captures.flatMap { moveVector =>
      getValidCaptureAlongVector(piece, pieceLocation, board, moveVector)  
    }
  }

  @tailrec def getValidCaptureAlongVector(
    piece: Piece,
    location: Location,
    board: BoardWithPieces,
    moveVector: MoveVector
  ): Option[Location] = {

    val nextSquareLocation = location.translate(moveVector)
    val nextSquare = board.getSquare(nextSquareLocation)

    nextSquare match {
      case None => None
      case Some(square) => {
        if (!square.canMoveOnto) None
        else if (board.pieces.get(nextSquareLocation).exists(p => p.color == piece.color)) None
        else if (board.pieces.get(nextSquareLocation).exists(p => p.color != piece.color)) Some(nextSquareLocation)
        else if (!square.canPassThrough) None // can't pass through, and there's no piece there
        else if (!moveVector.infinite) None
        else {
          getValidCaptureAlongVector(piece, nextSquareLocation, board, moveVector)
        }
      }
    }
  }

  val defaultPieces: Map[Location, Piece] = Map(
    Location(0, 0) -> Piece(White, King),
    Location(1, 3) -> Piece(Black, King),
    Location(4, 5) -> Piece(White, Bishop),
    Location(3, 4) -> Piece(White, Rook),
    Location(2, 3) -> Piece(Black, Knight)
  )
}
