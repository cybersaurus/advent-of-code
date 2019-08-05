package cybersaurus.adventofcode.aoc2018.day03

import cybersaurus.adventofcode.Input.readLines
import cybersaurus.adventofcode.aoc2018.day03.domain.Claim
import org.scalatest.{Matchers, WordSpec}

class Day03Spec extends WordSpec with Matchers {
  "Area" when {
    "invoking apply" should {
      "create a 3x2 grid" in {
        val r = Area((1, 1), 3, 2) should contain theSameElementsAs (Seq((1, 1), (2, 1), (3, 1), (1, 2), (2, 2), (3, 2)))
      }
    }
  }

  "Day03" when {
    val testInstance = Day03

    "invoking parseClaim" should {
      "parse a valid input line" in {
        val claim = testInstance.parseClaim("#1308 @ 1,1: 3x2")
        claim._1 shouldBe 1308
        claim._2 should contain theSameElementsAs (Seq((1, 1), (2, 1), (3, 1), (1, 2), (2, 2), (3, 2)))
      }
    }
  }

  "Day03Simple" when {
    val testInstance = new Day03Simple {}

    "overlappingSquaresCount" should {
      "return 0 squares" in {
        testInstance.overlappingSquaresCount(Seq(Area((1,1),1,1), Area((2,1),1,1))) shouldBe 0
      }

      "return 6 squares" in {
        testInstance.overlappingSquaresCount(Seq(Area((1,1),4,4), Area((2,3),4,4))) shouldBe 6
      }

      "return 113576 from input file" in {
        val areas = testInstance.parseSquares(readLines("2018/day03/input.txt"))
        testInstance.overlappingSquaresCount(areas) shouldBe 113576
      }
    }

    "findIntact" should {
      "find no claim id from empty input" in {
        testInstance.findIntact(Seq.empty) shouldBe None
      }

      "find claim id 123 from single Area input" in {
        testInstance.findIntact(Seq((123, Area((1,1),4,4)))) shouldBe Some(123)
      }

      "find claim id 3 intact" in {
        val claims: Seq[Claim] = Seq(
          (1, Area((1,3),4,4)),
          (2, Area((3,1),4,4)),
          (3, Area((5,5),2,2)),
        )
        testInstance.findIntact(claims) shouldBe Some(3)
      }

      // Ignored due to being too slow - approx 25 seconds.
      "return intact claim id 825 from the input file" ignore {
        val claims = testInstance.parseClaims(readLines("2018/day03/input.txt"))
        testInstance.findIntact(claims) shouldBe Some(825)
      }
    }
  }

  "Day03Grid" when {
    val testInstance = new Day03Grid {}

    "overlappingSquaresCount" should {
      "return 0 squares" in {
        val claims: Seq[Claim] = Seq(
          (1, Area((1,1),1,1)),
          (2, Area((2,1),1,1)),
        )
        val grid = testInstance.grid(claims)
        testInstance.overlappingSquaresCount(grid) shouldBe 0
      }

      "return 6 squares" in {
        val claims: Seq[Claim] = Seq(
          (1, Area((1,1),4,4)),
          (2, Area((2,3),4,4)),
        )
        val grid = testInstance.grid(claims)
        testInstance.overlappingSquaresCount(grid) shouldBe 6
      }

      "return 113576 from input file" in {
        val claims = Day03.parseClaims(readLines("2018/day03/input.txt"))
        val grid = testInstance.grid(claims)

        testInstance.overlappingSquaresCount(grid) shouldBe 113576
      }
    }

    "findIntact" should {
      "find no claim id from empty input" in {
        val claims = Seq.empty
        val grid = testInstance.grid(claims)

        testInstance.findIntact(claims, grid) shouldBe None
      }

      "find claim id 123 from single Area input" in {
        val claims: Seq[Claim] = Seq(
          (123, Area((1,1),4,4)),
        )
        val grid = testInstance.grid(claims)

        testInstance.findIntact(claims, grid) shouldBe Some(123)
      }

      "find claim id 3 intact" in {
        val claims: Seq[Claim] = Seq(
          (1, Area((1,3),4,4)),
          (2, Area((3,1),4,4)),
          (3, Area((5,5),2,2)),
        )
        val grid = testInstance.grid(claims)

        testInstance.findIntact(claims, grid) shouldBe Some(3)
      }

      "return intact claim id 825 from the input file" in {
        val claims = testInstance.parseClaims(readLines("2018/day03/input.txt"))
        val grid = testInstance.grid(claims)

        testInstance.findIntact(claims, grid) shouldBe Some(825)
      }
    }
  }
}
