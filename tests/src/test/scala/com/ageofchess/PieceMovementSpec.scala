package com.ageofchess

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._

class PieceMovementSpec extends AnyWordSpec with Matchers {

  val openBoard = Board(Vector.fill(8)(Vector.fill(8)(Terrain)))

  def bwp(pieces: Map[Location, Piece], squares: Vector[Vector[SquareType]] = openBoard.squares) =
    BoardWithPieces(squares, pieces, Set.empty)

  "validMoves" should {

    "slide a rook to the end of each axis on an open board" in {
      val moves = validMoves(bwp(Map(Location(3, 3) -> Piece(White, Rook))), Location(3, 3), Piece(White, Rook))
      moves should contain (Location(3, 7)) // right edge
      moves should contain (Location(3, 0)) // left edge
      moves should contain (Location(0, 3)) // top edge
      moves should contain (Location(7, 3)) // bottom edge
    }

    "stop a rook one square before a friendly piece and not go past it" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Rook),
        Location(3, 5) -> Piece(White, Pawn)
      )
      val moves = validMoves(bwp(pieces), Location(3, 3), Piece(White, Rook))
      moves     should contain     (Location(3, 4))
      moves should not contain Location(3, 5) // blocked by friendly
      moves should not contain Location(3, 6) // past the block
    }

    "stop a rook before an enemy piece (capture is handled separately)" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Rook),
        Location(3, 5) -> Piece(Black, Pawn)
      )
      val moves = validMoves(bwp(pieces), Location(3, 3), Piece(White, Rook))
      moves     should contain     (Location(3, 4))
      moves should not contain Location(3, 5) // enemy square is a capture, not a move
      moves should not contain Location(3, 6) // past the block
    }

    "allow a rook to move onto a Mine square but not slide past it" in {
      val squaresWithMine = openBoard.squares
        .updated(3, openBoard.squares(3).updated(5, Mine))
      val moves = validMoves(bwp(Map(Location(3, 3) -> Piece(White, Rook)), squaresWithMine), Location(3, 3), Piece(White, Rook))
      moves     should contain     (Location(3, 5)) // Mine is reachable
      moves should not contain Location(3, 6)       // but cannot slide past it
    }

    "not allow a rook to move onto Rocks" in {
      val squaresWithRocks = openBoard.squares
        .updated(3, openBoard.squares(3).updated(5, Rocks))
      val moves = validMoves(bwp(Map(Location(3, 3) -> Piece(White, Rook)), squaresWithRocks), Location(3, 3), Piece(White, Rook))
      moves should not contain Location(3, 5) // Rocks are impassable
      moves should not contain Location(3, 6) // blocked beyond Rocks too
    }

    "keep a bishop strictly on diagonals" in {
      val moves = validMoves(bwp(Map(Location(4, 4) -> Piece(White, Bishop))), Location(4, 4), Piece(White, Bishop))
      moves should contain     (Location(7, 7)) // NE diagonal
      moves should contain     (Location(1, 1)) // SW diagonal
      moves should not contain  Location(4, 5)  // orthogonal — not a bishop move
      moves should not contain  Location(5, 4)  // orthogonal
    }

    "allow a knight to jump over intervening pieces" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Knight),
        Location(3, 4) -> Piece(White, Pawn),  // directly adjacent, but knight jumps
        Location(4, 3) -> Piece(Black, Pawn)   // directly adjacent, but knight jumps
      )
      val moves = validMoves(bwp(pieces), Location(3, 3), Piece(White, Knight))
      moves should contain (Location(5, 4)) // valid L-shape regardless of intervening pieces
      moves should contain (Location(1, 4))
    }

    "give a king exactly 8 moves from an open interior square" in {
      val moves = validMoves(bwp(Map(Location(4, 4) -> Piece(White, King))), Location(4, 4), Piece(White, King))
      moves should have size 8
    }

    "limit a king to 3 moves from a corner" in {
      val moves = validMoves(bwp(Map(Location(0, 0) -> Piece(White, King))), Location(0, 0), Piece(White, King))
      moves should have size 3
    }
  }

  "validCaptures" should {

    "allow a rook to capture an enemy piece that is in its line" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Rook),
        Location(3, 6) -> Piece(Black, Pawn)
      )
      val captures = validCaptures(bwp(pieces), Location(3, 3), Piece(White, Rook))
      captures should contain (Location(3, 6))
    }

    "not allow a rook to capture through a blocking piece" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Rook),
        Location(3, 5) -> Piece(White, Pawn), // blocker
        Location(3, 6) -> Piece(Black, Pawn)  // target behind blocker
      )
      val captures = validCaptures(bwp(pieces), Location(3, 3), Piece(White, Rook))
      captures should not contain Location(3, 6)
    }

    "not allow a rook to capture a friendly piece" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Rook),
        Location(3, 6) -> Piece(White, Pawn)
      )
      val captures = validCaptures(bwp(pieces), Location(3, 3), Piece(White, Rook))
      captures should not contain Location(3, 6)
    }

    "allow a pawn to capture diagonally but not the orthogonal square it can move to" in {
      val pieces = Map(
        Location(3, 3) -> Piece(White, Pawn),
        Location(3, 4) -> Piece(Black, Pawn),  // orthogonal — Pawn moves here, does not capture
        Location(4, 4) -> Piece(Black, Knight)  // diagonal — Pawn captures here
      )
      val captures = validCaptures(bwp(pieces), Location(3, 3), Piece(White, Pawn))
      captures should not contain Location(3, 4) // orthogonal is movement, not capture
      captures should contain     (Location(4, 4))
    }

    "not allow capturing into Rocks" in {
      val squaresWithRocks = openBoard.squares
        .updated(3, openBoard.squares(3).updated(5, Rocks))
      val piecesAtRocks = Map(
        Location(3, 3) -> Piece(White, Rook)
      )
      val captures = validCaptures(
        BoardWithPieces(squaresWithRocks, piecesAtRocks, Set.empty),
        Location(3, 3),
        Piece(White, Rook)
      )
      captures should not contain Location(3, 5) // Rocks — canMoveOnto = false
    }
  }
}
