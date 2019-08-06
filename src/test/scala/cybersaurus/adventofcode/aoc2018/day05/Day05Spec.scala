package cybersaurus.adventofcode.aoc2018.day05

import cybersaurus.adventofcode.Input.readLines
import org.scalatest.{Matchers, WordSpec}


class Day05Spec extends WordSpec with Matchers {

  "reactions" should {
    val testInstance = new Day05 {}

    "react polymer dabAcCaCBAcCcaDA" in {
      testInstance.reactions("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
    }

    "react input polymer to length 11298" in {
      testInstance.reactions(readLines("2018/day05/input.txt").head).length shouldBe 11298
    }
  }
}
