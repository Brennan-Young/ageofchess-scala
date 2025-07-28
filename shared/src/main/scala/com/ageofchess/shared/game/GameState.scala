package com.ageofchess.shared.game

import com.ageofchess.shared.piece._
import com.ageofchess.shared.board._

case class GameState(
  gameId: String,
  white: Player,
  black: Player,
  board: Board,
  pieces: Map[Location, Piece],
  gold: Map[Player, Int],
  treasures: Set[Location],
  playerToMove: Player
) {

  def nextPlayer: Player = {
    if (playerToMove == white) black else white
  }

  def isValidMove(
    player: Player,
    playerAction: PlayerAction
  ): Boolean = {

    playerAction match {
      case PieceMove(from, to) => player == playerToMove && isValidPieceMove(from, to)
      case PiecePlacement(piece, to) => player == playerToMove && isValidPiecePlacement(player, piece, to)
    }
  }

  private def isValidPieceMove(
    from: Location,
    to: Location
  ): Boolean = {

    val piece = pieces.get(from)

    piece.exists { p =>
      val b = BoardWithPieces(board.squares, pieces, treasures)
      val moves = validMoves(b, from, p)
      val captures = validCaptures(b, from, p)

      moves.contains(to) || captures.contains(to)  
    }
  }

  private def isValidPiecePlacement(
    player: Player, 
    piece: Piece,
    to: Location
  ): Boolean = {

    val b = BoardWithPieces(board.squares, pieces, treasures)

    validPiecePlacements(b, piece).contains(to) && gold.get(player).exists(playerGold => playerGold >= piece.pieceType.value)
  }

  def validateAndGenerateNextState(
    player: Player,
    playerAction: PlayerAction
  ): Option[GameState] = {

    val updatedPieces = playerAction match {
      case PieceMove(from, to) => {
        pieces.get(from).map { piece => 
          pieces - from + (to -> piece)
        }
      }
      case PiecePlacement(piece, to) => {
        Some(pieces + (to -> piece))
      }
    }

    val updatedGold = playerAction match {
      case PieceMove(from, to) => {
        if (treasures.contains(playerAction.to)) {
          gold.get(playerToMove).map { playerGold =>
            gold.updated(playerToMove, playerGold + TreasureValue)  
          }
        } else Some(gold)
      }
      case PiecePlacement(piece, to) => {
        if (treasures.contains(playerAction.to)) {
          gold.get(playerToMove).map { playerGold =>
            gold.updated(playerToMove, playerGold - piece.pieceType.value + TreasureValue)
          }
        } else {
          gold.get(playerToMove).map { playerGold =>
            gold.updated(playerToMove, playerGold - piece.pieceType.value)  
          }
        }
      }
    }

    val updatedTreasures = if (treasures.contains(playerAction.to)) {
      treasures - playerAction.to
    } else treasures

    for {
      nextPieces <- updatedPieces
      nextGold <- updatedGold
      if isValidMove(player, playerAction)
    } yield {
      this.copy(
        pieces = nextPieces,
        gold = nextGold,
        treasures = updatedTreasures,
        playerToMove = nextPlayer
      )
    }
  }
}

trait PlayerAction {
  def to: Location
}

case class PieceMove(from: Location, to: Location) extends PlayerAction
case class PiecePlacement(piece: Piece, to: Location) extends PlayerAction