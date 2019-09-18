package cybersaurus.adventofcode.aoc2018.day06

object Day06 {
  case class Coord(x: Int, y: Int)

  object Coord {
    def apply(line: String): Coord = {
      val coord = """^([-]?\d+),[ ]?([-]?\d+)$""".r

      line match {
        case coord(x,y) => Coord(x.toInt, y.toInt)
      }
    }
  }

  private final val origin = Coord(0,0)

  case class GridPointAndClosestCoords(gridPoint: Coord, closestInputCoords: Iterable[Coord], manhattenDistance: Int)
  // Part 1


  // - Calculate size of grid from input.
  // - Filter out edge points from grid (and input coords)
  // - Iterate over each grid position
  // - Iterate over each input coord
  // - Calculate Manhatten Distance between each grid point and input coord
  // - If less than or equal to current, then update
  // - Remove results from all edge grid positions.
  // - Group by remaining input coords
  // - Find collection by max size.

  def toCoords(input: Seq[String]): Seq[Coord] = input map Coord.apply

  def maxBounds(coords: Iterable[Coord]): Coord =
    coords.foldLeft(origin) { case (acc, curr) =>
      Coord(Math.max(acc.x, curr.x), Math.max(acc.y, curr.y))
    }

  def grid(outerCoord: Coord, origin: Coord = origin): Seq[Coord] =
    for {
      x <- origin.x to outerCoord.x
      y <- origin.y to outerCoord.y
    } yield Coord(x,y)


  def manhattenDistance(a: Coord)(b: Coord): Int = Math.abs(a.x-b.x) + Math.abs(a.y-b.y)

  def closestInputCoordsForGridPoint(inputCoords: Iterable[Coord])(gridPoint: Coord): (Iterable[Coord], Int) = {

    val empty = (Set.empty[Coord], Int.MaxValue)

    val (closestCoords, dist) = inputCoords.foldLeft(empty) { case ((accCoords, accDist), inputCoord) =>
      val dist = manhattenDistance(gridPoint)(inputCoord)

      if (dist < accDist) (Set(inputCoord), dist)
      else if (dist == accDist) (accCoords + inputCoord, accDist)
      else (accCoords, accDist)
    }

    (closestCoords, dist)
  }

  def isGridEdgePoint(maxBounds: Coord, minBounds: Coord = origin)(gridPoint: Coord): Boolean =
    gridPoint.x == minBounds.x || gridPoint.x == maxBounds.x ||
    gridPoint.y == minBounds.y || gridPoint.y == maxBounds.y

  def multipleClosestInputPoints(gpacc: GridPointAndClosestCoords): Boolean = gpacc.closestInputCoords.size > 1

  def biggestArea(gpccs: Set[GridPointAndClosestCoords], isGridEdgePointFn: Coord => Boolean): (Coord, Int) = {
    val (coord, closestPoints) = gpccs
      .filterNot { multipleClosestInputPoints }
      .groupBy { gpcc => gpcc.closestInputCoords.head }
      .mapValues { gpccs2 => gpccs2.map{_.gridPoint} }
      .filterNot { case (inputCoord, closestGridPoints) => closestGridPoints.exists { isGridEdgePointFn }}
      .maxBy { case (p,closestPoints) => closestPoints.size }

    (coord, closestPoints.size)
  }

  def findSizeOfLandingArea(input: Seq[String]): (Coord, Int) = {
    val inputCoords = toCoords(input)
    val outerCoord = maxBounds(inputCoords)

    val isGridEdgePointFn: Coord => Boolean = isGridEdgePoint(outerCoord) _
    val closestInputCoordsForGridPointFn = closestInputCoordsForGridPoint(inputCoords) _

    val closestForGridPoints: Seq[GridPointAndClosestCoords] =
      grid(outerCoord)
        .map { gridPoint =>
          val (closestPoints, dist) = closestInputCoordsForGridPointFn(gridPoint)
          GridPointAndClosestCoords(gridPoint, closestPoints, dist)
        }

    biggestArea(closestForGridPoints.toSet, isGridEdgePointFn)
  }

  // Part 2

  // - For each grid point
  // - For each input coord
  // - Calculate MD
  // - Filter where sum is less than 10000
  // - Get size of remaining collection

  private[day06] def calculateManhattenDistanceSum(inputCoords: Seq[Coord])(gridPoint: Coord): Int =
    inputCoords
      .map { manhattenDistance(gridPoint) }
      .sum


  def findSizeOfAreaForSummedManhattenDistanceThreshold(input: Seq[String], mdThreshold: Int): Int = {
    val inputCoords = toCoords(input)

    grid(maxBounds(inputCoords))
      .map { calculateManhattenDistanceSum(inputCoords) }
      .filter { _ < mdThreshold }
      .size
  }
}
