package cybersaurus.adventofcode.aoc2018.day01

import org.scalatest.{MustMatchers, WordSpec}


class Day01Spec extends WordSpec with MustMatchers {

  "Day01" should {
    val changes = Day01.changes("2018/day01/input.txt")

    "changes" in {
      changes.size mustBe 959
    }

    "frequency" in {
      Day01.frequency(changes) mustBe 445
    }
  }

  "RecursiveDay01" should {
    val changes = Seq(1,2,3,4,-5)

    val testInstance = new RecursiveDay01()

    "firstRepeat" in {
      testInstance.firstRepeat(changes) mustBe 6
    }
  }

  "FoldLeftSlowDay01" should {
    val changes = Seq(1,2,3,4,-5)

    val testInstance = new FoldLeftSlowDay01()

    "firstRepeat" in {
      testInstance.firstRepeat(changes) mustBe 6
    }
  }

  "FoldLeftFastDay01" should {
    val changes = Day01.changes("2018/day01/input.txt")

    val testInstance = new FoldLeftFastDay01()

    "firstRepeat" in {
      testInstance.firstRepeat(changes) mustBe 219
    }
  }

  "ScanLeftDay01" should {
    val changes = Day01.changes("2018/day01/input.txt")

    val testInstance = new ScanLeftDay01()

    "firstRepeat" in {
      testInstance.firstRepeat(changes) mustBe 219
    }
  }
}
