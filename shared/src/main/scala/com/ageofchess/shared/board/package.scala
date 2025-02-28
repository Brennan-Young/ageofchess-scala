package com.ageofchess.shared

import upickle.default.{ReadWriter, macroRW}

package object board {
  sealed trait SquareColor {
    def id: String
  }

  case object Dirt extends SquareColor { override def id: String = "dirt" }
  case object Grass extends SquareColor { override def id: String = "grass" }

  sealed trait SquareType {
    def id: String
  }
  object SquareType {
    implicit val rw: ReadWriter[SquareType] = macroRW
  }

  case object Terrain extends SquareType { 
    override def id: String = "base"
    implicit val rw: ReadWriter[Terrain.type] = macroRW
  }
  case object Mine extends SquareType {
    override def id: String = "mine"
    implicit val rw: ReadWriter[Mine.type] = macroRW
  }
  case object Trees extends SquareType {
    override def id: String = "trees"
    implicit val rw: ReadWriter[Trees.type] = macroRW
  }
  case object Rocks extends SquareType {
    override def id: String = "rocks"
    implicit val rw: ReadWriter[Rocks.type] = macroRW
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

  
}
