package com.ageofchess.shared 

trait SquareType {
  def canMoveOnto: Boolean
  def canPassThrough: Boolean
  def value: Int
}

case object Terrain extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = true
  override def value: Int = 0
}

case object Rock extends SquareType {
  override def canMoveOnto: Boolean = false
  override def canPassThrough: Boolean = false
  override def value: Int = 0
}

case object Tree extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = false
  override def value: Int = 0
}

case object Resource extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = false
  override def value: Int = 3
}

case object Treasure extends SquareType {
  override def canMoveOnto: Boolean = true
  override def canPassThrough: Boolean = false
  override def value: Int = 20
}
