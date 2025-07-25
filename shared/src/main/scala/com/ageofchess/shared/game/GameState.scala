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

  def computeNextState(
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

    val updatedGold = if (treasures.contains(playerAction.to)) {
      gold.get(playerToMove).map { playerGold =>
        gold.updated(playerToMove, playerGold + TreasureValue)  
      }
    } else Some(gold)

    val updatedTreasures = if (treasures.contains(playerAction.to)) {
      treasures - playerAction.to
    } else treasures

    for {
      nextPieces <- updatedPieces
      nextGold <- updatedGold
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