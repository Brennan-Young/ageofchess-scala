package com.ageofchess.server

import com.ageofchess.shared.game.GameState
import com.ageofchess.shared.game.Player
import com.ageofchess.shared.game.PlayerClock

case class ActiveGame(
  gameState: GameState,
  clocks: Map[Player, PlayerClock]
)
