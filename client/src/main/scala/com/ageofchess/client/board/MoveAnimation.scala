package com.ageofchess.client.board

import com.ageofchess.shared.piece.{Location, Piece}

case class AnimatingMove(piece: Piece, from: Location, to: Location)
