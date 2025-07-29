package com.ageofchess.shared.board

object BoardGenerator {

  /* 
   * Future: parameterize symmetry, number of mines, forest count, mountain count
   * Make sure that mountains can't create 
   */
  def generateBoard(
    size: Int,
    minesPerSide: Int = 3
  ): Board = {

    Board(Vector())
  }
}
