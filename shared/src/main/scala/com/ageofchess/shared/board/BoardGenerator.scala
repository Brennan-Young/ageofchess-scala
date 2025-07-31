package com.ageofchess.shared.board

import scala.util.Random
import com.ageofchess.shared.piece.Location
import scala.collection.mutable

object BoardGenerator {

  /* 
   * Future: parameterize symmetry, number of mines, forest count, mountain count
   * Make sure that mountains can't create 
   */
  def generateBoard(
    size: Int,
    minesPerSide: Int = 3,
    seedForestProbability: Double = 0.05,
    seedMountainProbability: Double = 0.02,
    random: Random = new Random()
  ): Board = {

    val terrainMap: mutable.Map[Location, SquareType] = mutable.Map()

    val half = size / 2

    def inBounds(loc: Location): Boolean = {
      loc.row >= -half && loc.row < size - half &&
      loc.col >= -half && loc.col < size - half
    }

    for {
      row <- 0 until half
      col <- 0 until size
    } {

      val loc = Location(row, col)

      if (random.nextDouble() < seedForestProbability) {
        println("generating forest at " + loc)
        val forest = growForest(loc, maxSize = 8, spreadChance = 0.2, random)
        forest.foreach { tree =>
          terrainMap += (tree -> Trees)
        }
      } else if (random.nextDouble() < seedMountainProbability) {
        println("generating mountain at " + loc)
        val mountainRange = growMountainRange(loc, length = 6, random)
        mountainRange.foreach { mountain =>
          terrainMap += (mountain -> Rocks)  
        }
      }
    }

    val mineLocations: mutable.Set[Location] = mutable.Set()

    val mines = while (mineLocations.size < minesPerSide) {
      val r = random.nextInt(half)
      val c = random.nextInt(size)

      val loc = Location(r, c)

      mineLocations += loc
    }

    terrainMap ++= mineLocations.map(loc => (loc -> Mine))

    val x: Vector[Vector[SquareType]] = Vector.tabulate(half, size) { (row, col) =>
      terrainMap.getOrElse(Location(row, col), Terrain)
    }

    val y = x.map { row => row.reverse }.reverse

    Board(x ++ y)
  }

  // growForest and growMountainRange are gen-AI code - revisit them for tune up
  def growForest(
    startingLocation: Location,
    maxSize: Int,
    spreadChance: Double,
    random: Random
  ): Set[Location] = {
    val forest = scala.collection.mutable.Set(startingLocation)
    val frontier = scala.collection.mutable.Queue(startingLocation)

    while (forest.size < maxSize && frontier.nonEmpty) {
      val current = frontier.dequeue()
      val neighbors = List(
        Location(current.row + 1, current.col),
        Location(current.row - 1, current.col),
        Location(current.row, current.col + 1),
        Location(current.row, current.col - 1)
      )

      neighbors.foreach { n =>
        if (!forest.contains(n) && random.nextDouble() < spreadChance) {
          forest += n
          frontier.enqueue(n)
        }
      }
    }

    println("Forest: " + forest)

    forest.toSet
  }

  def growMountainRange(
    start: Location,
    length: Int,
    rand: Random
  ): Set[Location] = {
    val directions = List(
      Location(1, 0), Location(-1, 0),
      Location(0, 1), Location(0, -1)
    )

    val range = scala.collection.mutable.Set(start)
    var current = start
    val mainDir = directions(rand.nextInt(directions.size))

    for (_ <- 1 to length) {
      // Move mostly in main direction, with a chance of deviation
      val dir = if (rand.nextDouble() < 0.5) mainDir
                else directions(rand.nextInt(directions.size))

      val next = Location(current.row + dir.row, current.col + dir.col)
      range += next
      current = next

      // Small branch with 10% chance
      if (rand.nextDouble() < 0.1) {
        val branchDir = directions(rand.nextInt(directions.size))
        val branch = Location(current.row + branchDir.row, current.col + branchDir.col)
        range += branch
      }
    }

    println("Mountain: " + range)

    range.toSet
  }
}
