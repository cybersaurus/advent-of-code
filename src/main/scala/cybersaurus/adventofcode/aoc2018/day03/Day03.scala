package cybersaurus.adventofcode.aoc2018.day03

import cybersaurus.adventofcode.aoc2018.day03.domain.{Area, Claim, Coord}

package object domain {
  type Coord = (Int,Int)
  type Area = Seq[Coord]
  type Claim = (Int, Area)
}

object Area {

  def apply(start: Coord, width: Int, height: Int): Area =
    for {
      xOffset <- (0 until width)
      yOffset <- (0 until height)
      (x,y)    = start
    } yield (x + xOffset, y + yOffset)
}

trait Day03 {
  private val pattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

  def parseClaim(line: String): Claim = {
    val pattern(id, x, y, w, h) = line

    (id.toInt, Area((x.toInt, y.toInt), w.toInt, h.toInt))
  }

  def parseClaims(lines: Seq[String]): Seq[Claim] = lines map parseClaim

  def parseSquares(lines: Seq[String]): Seq[Area] = parseClaims(lines).map(_._2)
}
object Day03 extends Day03

// Simple but very slow (approx 25s for input.txt)
trait Day03Simple extends Day03 {

  // Part 1
  //
  def overlappingSquaresCount(areas: Seq[Area]): Int =
    areas
      .flatten
      .groupBy { identity }
      .toSeq
      .filter { case (coord, matches) => matches.size > 1 }
      .size

  // Part 2
  //
  def findIntact(claims: Seq[Claim]): Option[Int] = {
    def overlapping(a1: Area, a2: Area): Boolean = (a1 == a2) || (a1 intersect a2).nonEmpty

    claims
      .find { case (currId,currArea) =>
        claims.forall {
          case (id,area) =>
            currId == id || !overlapping(currArea, area)
        }
      }
      .map{_._1}
  }
}

class Day03Grid extends Day03 {

  def grid(claims: Seq[Claim]): Array[Array[Int]] = {
    val grid: Array[Array[Int]] = Array.ofDim[Int](1000,1000)

    claims.foreach { claim =>
      claim._2.foreach { case (x, y) =>
        grid(x)(y) = grid(x)(y) + 1
      }
    }
/*
    FAILS! - Only lazily processes the first element of the Stream.
    Return type of the for-comprehension is Stream[Unit].

    for {
      claim  <- claims
      (x, y) <- claim._2
    } yield grid(x)(y) = grid(x)(y) + 1
*/
    grid
  }

  // Part 1
  //
  def overlappingSquaresCount(grid: Array[Array[Int]]): Int =
    grid.foldLeft(0) { case (acc,row) => acc + row.count { squaresOnPoint => squaresOnPoint > 1 } }

  // Part 2
  //
  def findIntact(claims: Seq[Claim], grid: Array[Array[Int]]): Option[Int] =
    claims
      .find { claim => claim._2.forall { case (x,y) => grid(x)(y) == 1 } }
      .map { claim => claim._1 }
}
