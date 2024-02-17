package tld.ageofchess.server.game

trait Piece {
  def value: Int

  def moves: Set[MoveVector]
  def captures: Set[MoveVector]
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

case class Location(x: Int, y: Int)

trait Square {
  def location: Location
  def occupyingPiece: Option[Piece]

  def canMoveOnto: Boolean
  def canPassThrough: Boolean
  def value: Int = 0
}

trait Terrain extends Square {
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

case class Board(squares: Vector[Vector[Square]]) {
  def height = squares.size
  def width = squares.headOption.map(_.size).getOrElse(0)
}

trait Symmetry
case object NoSymmetry extends Symmetry
case object Horizontal extends Symmetry
case object Vertical extends Symmetry
case object Rotational extends Symmetry

object Board {
  def generate(
    height: Int,
    width: Int
  ): Board = {
    val x = (0 until height).map { i =>
      (0 until width).map { j =>
        new Terrain {
          override def location = Location(i, j)
          override def occupyingPiece: Option[Piece] = None
        }
      }.toVector
    }.toVector

    Board(x)
  }
}

object Main extends App {
  println("hello world")
}