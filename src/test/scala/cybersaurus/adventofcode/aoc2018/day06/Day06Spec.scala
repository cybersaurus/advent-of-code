package cybersaurus.adventofcode.aoc2018.day06

import cybersaurus.adventofcode.Input.readLines
import cybersaurus.adventofcode.aoc2018.day06.Day06.{Coord, GridPointAndClosestCoords}
import org.scalatest.{Matchers, WordSpec}

class Day06Spec extends WordSpec with Matchers {
  // Part 1

  "parse input line" should {
    "parse (137, 282)" in {
      Coord.apply("137, 282") shouldBe Coord(137, 282)
    }
    "parse (137, 282) - with no spaces" in {
      Coord.apply("137,282") shouldBe Coord(137, 282)
    }
    "parse negative" in {
      Coord.apply("-137, -282") shouldBe Coord(-137, -282)
    }
  }

  "maxBounds" should {
    "return (3,5)" in {
      Day06.maxBounds(Set(Coord(0, 0), Coord(3, 2), Coord(2, 5))) shouldBe Coord(3, 5)
    }
  }

  "isGridEdgePoint" should {
    val isGridEdgePointFn = Day06.isGridEdgePoint(Coord(4, 4)) _

    "detect point on x-axis" in {
      isGridEdgePointFn(Coord(0, 4)) shouldBe true
    }
    "detect point on y-axis" in {
      isGridEdgePointFn(Coord(4, 0)) shouldBe true
    }
    "detect point within grid" in {
      isGridEdgePointFn(Coord(2, 2)) shouldBe false
    }
    "detect point outside grid" in {
      isGridEdgePointFn(Coord(9, 0)) shouldBe true
    }
  }

  "closestInputCoordsForGridPoint" should {
    val closestInputCoordsForGridPointFn = Day06.closestInputCoordsForGridPoint(Set(Coord(1,1), Coord(1,11), Coord(6,7))) _

    "return (5,5) with manhatten distance 2" in {
      closestInputCoordsForGridPointFn(Coord(4,6)) shouldBe (Set(Coord(6,7)), 3)
    }

    "return (1,1) and (1,11) with manhatten distance 5" in {
      closestInputCoordsForGridPointFn(Coord(1,6)) shouldBe (Set(Coord(1,1), Coord(1,11)), 5)
    }
  }

  "biggestArea" should {
    val isGridEdgePointFn = Day06.isGridEdgePoint(Coord(10,10)) _

    val inputCoordA = Coord(0,1)
    val inputCoordB = Coord(5,6)
    val inputCoordC = Coord(3,3)

    "find input coord closest to most grid points" in {
      Day06.biggestArea(
        Set(
          GridPointAndClosestCoords(Coord(0,0), Set(inputCoordA), -999),
          GridPointAndClosestCoords(Coord(5,5), Set(inputCoordB), -999),
          GridPointAndClosestCoords(Coord(4,4), Set(inputCoordB), -999),
          GridPointAndClosestCoords(Coord(3,3), Set(inputCoordC), -999),
          GridPointAndClosestCoords(Coord(1,1), Set(inputCoordB), -999),
        ),
        isGridEdgePointFn
      ) shouldBe (inputCoordB, 3)
    }

    "remove grid points with multiple closest input points" in {
      Day06.biggestArea(
        Set(
          GridPointAndClosestCoords(Coord(0,0), Set(inputCoordA, inputCoordB), -999),
          GridPointAndClosestCoords(Coord(5,5), Set(inputCoordA), -999),
          GridPointAndClosestCoords(Coord(4,4), Set(inputCoordA), -999),
          GridPointAndClosestCoords(Coord(2,2), Set(inputCoordB, inputCoordC), -999),
        ),
        isGridEdgePointFn
      ) shouldBe (inputCoordA, 2)
    }
  }

  "findSizeOfLandingArea" should {
    "calculate answer for example input" in {
      val input = Seq(
        "1, 1", // A
        "1, 6", // B
        "8, 3", // C
        "3, 4", // D
        "5, 5", // E
        "8, 9", // F
      )
      Day06.findSizeOfLandingArea(input) shouldBe (Coord(5,5), 17)
    }

    "calculate answer from test input" in {
      Day06.findSizeOfLandingArea(readLines("2018/day06/input.txt")) shouldBe (Coord(265,98), 4754)
    }
  }

  // Part 2

  "calculateManhattenDistanceSum" should {
    "calculate sum of 30" in {
      val inputCoords = Seq(
        Coord(1,1),
        Coord(1,6),
        Coord(8,3),
        Coord(3,4),
        Coord(5,5),
        Coord(8,9),
      )

      Day06.calculateManhattenDistanceSum(inputCoords)(Coord(4,3)) shouldBe 30
    }
  }

  "findSizeOfAreaForSummedManhattenDistanceThreshold" should {
    "calculate answer for example input" in {
      val input = Seq(
        "1, 1", // A
        "1, 6", // B
        "8, 3", // C
        "3, 4", // D
        "5, 5", // E
        "8, 9", // F
      )

      Day06.findSizeOfAreaForSummedManhattenDistanceThreshold(input, 32) shouldBe 16
    }

    "calculate answer for test input" in {
      Day06.findSizeOfAreaForSummedManhattenDistanceThreshold(readLines("2018/day06/input.txt"), 10000) shouldBe 42344
    }
  }
}
