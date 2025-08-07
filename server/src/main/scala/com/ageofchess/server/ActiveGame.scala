package com.ageofchess.server

import com.ageofchess.shared.game.GameState
import com.ageofchess.shared.user.{GameUser, Player, Spectator}
import com.ageofchess.shared.game.PlayerClock

case class ActiveGame(
  gameState: GameState,
  clocks: Map[Player, PlayerClock],
  players: List[Player],
  spectators: List[Spectator]
) {

  val allAssociatedClients: List[GameUser] = players ++ spectators
}
