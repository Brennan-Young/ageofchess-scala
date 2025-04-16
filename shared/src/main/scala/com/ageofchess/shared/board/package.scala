package com.ageofchess.shared

import upickle.default.{ReadWriter, readwriter, macroRW}
import com.ageofchess.shared.piece._

package object board {
  sealed trait SquareColor {
    def id: String
  }

  case object Dirt extends SquareColor { override def id: String = "dirt" }
  case object Grass extends SquareColor { override def id: String = "grass" }

  sealed trait SquareType {
    def id: String

    def canMoveOnto: Boolean
    def canPassThrough: Boolean
  }

  object SquareType {
    implicit val rw: ReadWriter[SquareType] = readwriter[String].bimap[SquareType](
      {
        case Terrain => "terrain"
        case Mine => "mine"
        case Trees => "trees"
        case Rocks => "rocks"
      },
      {
        case "terrain" => Terrain
        case "mine" => Mine
        case "trees" => Trees
        case "rocks" => Rocks
        case _ => throw new Exception
      }
    )
  }

  case object Terrain extends SquareType { 
    override def id: String = "base"

    override def canMoveOnto = true
    override def canPassThrough = true
  }
  case object Mine extends SquareType {
    override def id: String = "mine"

    override def canMoveOnto = true
    override def canPassThrough = false
  }
  case object Trees extends SquareType {
    override def id: String = "trees"

    override def canMoveOnto = true
    override def canPassThrough = false
  }
  case object Rocks extends SquareType {
    override def id: String = "rocks"

    override def canMoveOnto = false
    override def canPassThrough = false
  }

  case class RenderableSquare(color: SquareColor, squareType: SquareType) {
    def asset: String = s"${color.id}_${squareType.id}.png"
  }

  val defaultBoard: Board = Board(
    Vector(
      Vector(
        Terrain, Mine, Trees, Rocks
      ),
      Vector(
        Terrain, Mine, Trees, Rocks
      )
    )
  )

  val defaultBoard2: Board = Board(
    Vector(
      Vector(
        Terrain, Mine, Trees, Rocks, Terrain
      ),
      Vector(
        Terrain, Mine, Trees, Rocks, Terrain
      )
    )
  )

  val defaultBoard3: Board = Board(
    Vector(
      Vector(Terrain, Terrain, Terrain, Terrain, Terrain, Terrain),
      Vector(Terrain, Trees, Terrain, Terrain, Rocks, Terrain),
      Vector(Terrain, Terrain, Terrain, Terrain, Terrain, Terrain),
      Vector(Terrain, Terrain, Rocks, Rocks, Terrain, Terrain),
      Vector(Trees, Trees, Terrain, Terrain, Terrain, Terrain),
      Vector(Terrain, Terrain, Terrain, Terrain, Terrain, Terrain)
    )
  )

  val defaultRenderableBoard = defaultBoard.toRenderable

  case class Board(squares: Vector[Vector[SquareType]]) {
    def toRenderable: Vector[Vector[RenderableSquare]] = {
      squares.zipWithIndex.map { case (row, rIdx) =>
        row.zipWithIndex.map { case (square, cIdx) =>
          val color = if ((rIdx + cIdx) % 2 == 0) Grass else Dirt
          RenderableSquare(color, square)
        }  
      }
    }
  }

  object Board {
    implicit val rw: ReadWriter[Board] = macroRW
  }

  case class BoardWithPieces(
    squares: Vector[Vector[SquareType]],
    pieces: Map[Location, Piece]
  ) {

    def getSquare(square: Location): Option[SquareType] = {
      squares.lift(square.row).flatMap { row =>
        row.lift(square.col)
      }
    }
  }

  object BoardWithPieces {
    implicit val rw: ReadWriter[BoardWithPieces] = macroRW
  }

  
}
