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
  override def value = 20

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
  override def value = 30

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
  override def value = 25

  override def moves = Set(
    MoveVector(1, 1, true),
    MoveVector(1, -1, true),
    MoveVector(-1, -1, true),
    MoveVector(-1, 1, true)
  )

  override def captures = moves
}

trait Rook extends Piece {
  override def value = 35

  override def moves = Set(
    MoveVector(1, 0, true),
    MoveVector(0, 1, true),
    MoveVector(-1, 0, true),
    MoveVector(0, -1, true)
  )

  override def captures = moves
}

trait Queen extends Piece {
  override def value = 70

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

trait King extends Piece {
  override def value = 0

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

case object WhitePawn extends Pawn { override def color = White }
case object BlackPawn extends Pawn { override def color = Black }

case object WhiteKnight extends Knight { override def color = White }
case object BlackKnight extends Knight { override def color = Black }
case object WhiteBishop extends Bishop { override def color = White }
case object BlackBishop extends Bishop { override def color = Black }

case object WhiteRook extends Rook { override def color = White }
case object BlackRook extends Rook { override def color = Black }

case object WhiteQueen extends Queen { override def color = White }
case object BlackQueen extends Queen { override def color = Black }
case object WhiteKing extends King { override def color = White }
case object BlackKing extends King { override def color = Black }

case class Location(x: Int, y: Int) {
  def translate(vector: MoveVector): Location = {
    Location(x + vector.x, y + vector.y)
  }
}

trait SquareType {
  def canMoveOnto: Boolean
  def canPassThrough: Boolean
  def value: Int
}

case object Terrain extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = true
  override def value: Int = 0
}

case object Rock extends SquareType {
  override def canMoveOnto: Boolean = false
  override def canPassThrough: Boolean = false
  override def value: Int = 0
}

case object Tree extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = false
  override def value: Int = 0
}

case object Resource extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = false
  override def value: Int = 3
}

case object Treasure extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = false
  override def value: Int = 20
}

case class Square(
  squareType: SquareType,
  location: Location,
  occupyingPiece: Option[PlacedPiece]
)

// trait Square {
//   def location: Location
//   def occupyingPiece: Option[PlacedPiece]

//   def canMoveOnto: Boolean
//   def canPassThrough: Boolean
//   def value: Int = 0
// }

// case class Terrain(location: Location, occupyingPiece: Option[PlacedPiece]) extends Square {
//   override def canMoveOnto = true
//   override def canPassThrough = true
// }

// trait Rock extends Square {
//   override def canMoveOnto = false
//   override def canPassThrough = false
// }

// trait Tree extends Square {
//   override def canMoveOnto = true
//   override def canPassThrough = false
// }

// trait Resource extends Square {
//   override def canMoveOnto = true
//   override def canPassThrough = false
//   override def value = 3
// }

case class SparseBoard(height: Int, width: Int, squares: Map[Location, Square], pieces: Map[Location, PlacedPiece]) {
  def placePiece(location: Location, p: Piece): SparseBoard = {
    this.copy(pieces = this.pieces + (location -> PlacedPiece(location, p)))
  }
}

case class Player(color: Color, gold: Int)

trait PlayerAction {
  def player: Player
}

case class MovePiece(player: Player, source: Location, destination: Location) extends PlayerAction
case class CreatePiece(player: Player, loc: Location, piece: Piece) extends PlayerAction

case class GameState(
  white: Player,
  black: Player,
  board: SparseBoard,
  whiteToMove: Boolean
) {

  // Assume moves and creations are valid here.  Do validation one level up

  def nextState(
    action: PlayerAction
  ): GameState = {
    action match {
      case MovePiece(player, source, destination) => nextStateMove(player, source, destination)
      case CreatePiece(player, loc, piece) => nextStateCreate(player, source, destination)
    }
  }

  private def nextStateMove(player: Player, source: Location, destination: Location): GameState = {
    val nextPieces = board.pieces - source + (destination -> board.pieces.get(source).get)
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
      squares = this.square.updated(
        height - location.y, this.squares(height - location.y).updated(
          location.x - 1, this.get(location).get.copy(occupyingPiece = Some(p))
        )
      )
    )
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
        Square(Terrain, Location(j + 1, height - i), None)
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