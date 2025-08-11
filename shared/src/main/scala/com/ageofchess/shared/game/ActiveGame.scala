package com.ageofchess.shared.game

import com.ageofchess.shared.user.{GameUser, Player, Spectator}

case class ActiveGame(
  gameState: GameState,
  clocks: Map[Player, PlayerClock],
  players: List[Player],
  spectators: List[Spectator]
) {

  val allAssociatedClients: List[GameUser] = players ++ spectators
}
