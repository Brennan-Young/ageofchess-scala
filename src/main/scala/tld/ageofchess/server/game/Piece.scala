package tld.ageofchess.server.game

import scala.collection.mutable
import scala.annotation.tailrec

trait Color
case object White extends Color
case object Black extends Color

trait Piece {
  def value: Int
  def color: Color

  def moves: Set[MoveVector]
  def captures: Set[MoveVector]
}

case class PlacedPiece(location: Location, piece: Piece) {
  def validMoves(board: Board): Set[Square] = {
    piece.moves.flatMap { moveVector =>
      getValid(mutable.Set.empty[Square], board, location, moveVector)
    }
  }

  def validCaptures(board: Board): Set[Square] = {
    piece.captures.flatMap { moveVector =>
      getValid(mutable.Set.empty[Square], board, location, moveVector)
    }
      .filter { square =>
        square.occupyingPiece.exists { op =>
          op.piece.color != piece.color  
        }  
      }
  }

  @tailrec final def getValid(state: mutable.Set[Square], board: Board, location: Location, moveVector: MoveVector): Set[Square] = {
    val nextSquareLocation = location.translate(moveVector)
    val nextSquare = board.get(nextSquareLocation)

    nextSquare match {
      case None => state.toSet
      case Some(square) => {
        if (!square.canMoveOnto) state.toSet
        else if (!square.canPassThrough) (state + square).toSet
        else if (!moveVector.infinite) (state + square).toSet
        else {
          getValid(state + square, board, nextSquareLocation, moveVector)
        }
      }
    }
  }
}

case class MoveVector(
  x: Int,
  y: Int,
  infinite: Boolean
)

trait Pawn extends Piece {
  override def value = 25

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

trait Knight extends Piece {
  override def value = 35

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

trait Bishop extends Piece {
  override def value = 30

  override def moves = Set(
    MoveVector(1, 1, true),
    MoveVector(1, -1, true),
    MoveVector(-1, -1, true),
    MoveVector(-1, 1, true)
  )

  override def captures = moves
}

case object WhitePawn extends Pawn { override def color = White }
case object BlackPawn extends Pawn { override def color = Black }

case object WhiteKnight extends Knight { override def color = White }
case object BlackKnight extends Knight { override def color = Black }
case object WhiteBishop extends Bishop { override def color = White }
case object BlackBishop extends Bishop { override def color = Black }

case class Location(x: Int, y: Int) {
  def translate(vector: MoveVector): Location = {
    Location(x + vector.x, y + vector.y)
  }
}

trait Square {
  def location: Location
  def occupyingPiece: Option[PlacedPiece]

  def canMoveOnto: Boolean
  def canPassThrough: Boolean
  def value: Int = 0
}

case class Terrain(location: Location, occupyingPiece: Option[PlacedPiece]) extends Square {
  override def canMoveOnto = true
  override def canPassThrough = true
}

trait Rock extends Square {
  override def canMoveOnto = false
  override def canPassThrough = false
}

trait Tree extends Square {
  override def canMoveOnto = true
  override def canPassThrough = false
}

trait Resource extends Square {
  override def canMoveOnto = true
  override def canPassThrough = false
  override def value = 3
}

case class SparseBoard(height: Int, width: Int, squares: Map[Location, Square], pieces: Map[Location, PlacedPiece]) {
  def placePiece(location: Location, p: Piece): SparseBoard = {
    this.copy(pieces = this.pieces + (location -> PlacedPiece(location, p)))
  }
}

trait Symmetry
case object NoSymmetry extends Symmetry
case object Horizontal extends Symmetry
case object Vertical extends Symmetry
case object Rotational extends Symmetry


case class Board(squares: Vector[Vector[Square]]) {
  def height = squares.size
  def width = squares.headOption.map(_.size).getOrElse(0)

  def get(location: Location): Option[Square] = {
    squares.lift(height - location.y).flatMap(_.lift(location.x - 1))
  }

  def placePiece(location: Location, p: Piece): Board = {
    this.copy(
      squares = this.squares.updated(
        height - location.y, this.squares(height - location.y).updated(
          location.x - 1, this.get(location).get match {
            case Terrain(loc, None) => Terrain(loc, Some(PlacedPiece(location, p)))
          })))
  }
}

object Board {
  def generate(
    height: Int,
    width: Int
  ): Board = {
    val x = (0 until height).map { i =>
      (0 until width).map { j =>
        Terrain(Location(j + 1, height - i), None)
      }.toVector
    }.toVector

    Board(x)
  }
}

object Main extends App {
  val board = Board.generate(10, 10)

  val loc = Location(4, 5)

  val nextBoard = board.placePiece(Location(4, 5), WhiteKnight).placePiece(Location(6, 6), BlackPawn).placePiece(Location(2, 4), WhiteBishop)

  nextBoard.squares.foreach { row => println(row.map(_.location))}


  nextBoard.get(Location(4, 5)).foreach { square =>
    square.occupyingPiece.foreach { piece =>
      println(piece.validMoves(nextBoard))
      println(piece.validCaptures(nextBoard))  
    }
  }

  nextBoard.get(Location(2, 4)).foreach { square =>
    square.occupyingPiece.foreach { piece =>
      println(piece.validMoves(nextBoard))
      println(piece.validCaptures(nextBoard))  
    }
  }
}