package com.ageofchess

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import com.ageofchess.shared.game._
import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._
import com.ageofchess.shared.user._

class GameStateSpec extends AnyWordSpec with Matchers {

  val openBoard = Board(Vector.fill(8)(Vector.fill(8)(Terrain)))
  val white     = Player(UserId("white"), White)
  val black     = Player(UserId("black"), Black)

  def makeState(
    pieces:    Map[Location, Piece],
    gold:      Map[Player, Int]  = Map(white -> 100, black -> 100),
    treasures: Set[Location]     = Set.empty,
    toMove:    Player            = white
  ): GameState =
    GameState("test-game", white, black, openBoard, pieces, gold, treasures, toMove)

  val baseKings = Map(
    Location(4, 4) -> Piece(White, King),
    Location(0, 0) -> Piece(Black, King)
  )

  "validateAndGenerateNextState" should {

    "move a king to an adjacent square and advance the turn" in {
      val next = makeState(baseKings)
        .validateAndGenerateNextState(white, PieceMove(Piece(White, King), Location(4, 4), Location(4, 5)))
      next should not be empty
      next.get.pieces should contain     (Location(4, 5) -> Piece(White, King))
      next.get.pieces should not contain key (Location(4, 4))
      next.get.playerToMove shouldBe black
    }

    "reject a move by the player whose turn it is not" in {
      val next = makeState(baseKings)
        .validateAndGenerateNextState(black, PieceMove(Piece(Black, King), Location(0, 0), Location(0, 1)))
      next shouldBe None
    }

    "reject a move to a square the piece cannot reach" in {
      val next = makeState(baseKings)
        .validateAndGenerateNextState(white, PieceMove(Piece(White, King), Location(4, 4), Location(4, 6)))
      next shouldBe None
    }

    "place a piece adjacent to the king and deduct its cost" in {
      val pawn = Piece(White, Pawn)
      val next = makeState(baseKings, gold = Map(white -> 50, black -> 50))
        .validateAndGenerateNextState(white, PiecePlacement(pawn, Location(4, 5)))
      next should not be empty
      next.get.pieces    should contain (Location(4, 5) -> pawn)
      next.get.gold(white) shouldBe (50 - Pawn.value)
    }

    "reject a placement the player cannot afford" in {
      val next = makeState(baseKings, gold = Map(white -> 5, black -> 100))
        .validateAndGenerateNextState(white, PiecePlacement(Piece(White, Pawn), Location(4, 5)))
      next shouldBe None
    }

    "reject a placement on a square not adjacent to any friendly non-pawn" in {
      val next = makeState(baseKings)
        .validateAndGenerateNextState(white, PiecePlacement(Piece(White, Pawn), Location(7, 7)))
      next shouldBe None
    }

    "award TreasureValue gold when a piece moves onto a treasure square" in {
      val treasureLoc = Location(4, 5)
      val next = makeState(baseKings, treasures = Set(treasureLoc))
        .validateAndGenerateNextState(white, PieceMove(Piece(White, King), Location(4, 4), treasureLoc))
      next should not be empty
      next.get.gold(white)   shouldBe (100 + TreasureValue)
      next.get.treasures should not contain treasureLoc
    }

    "not award gold when moving to a square without a treasure" in {
      val next = makeState(baseKings)
        .validateAndGenerateNextState(white, PieceMove(Piece(White, King), Location(4, 4), Location(4, 5)))
      next should not be empty
      next.get.gold(white) shouldBe 100
    }

    "allow black to move on black's turn" in {
      val next = makeState(baseKings, toMove = black)
        .validateAndGenerateNextState(black, PieceMove(Piece(Black, King), Location(0, 0), Location(0, 1)))
      next should not be empty
      next.get.playerToMove shouldBe white
    }
  }
}
