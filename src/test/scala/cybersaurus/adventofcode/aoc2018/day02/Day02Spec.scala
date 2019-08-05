package cybersaurus.adventofcode.aoc2018.day02

import cybersaurus.adventofcode.Input.readLines

import org.scalatest.{MustMatchers, WordSpec}

class Day02Spec extends WordSpec with MustMatchers {
  private val testInstance = new Day02()

  "twosAndThreesMatches" should {
    "return (1,0) for aa" in {
      testInstance.twosAndThreeMatches("aa") mustBe (1,0)
    }

    "return (0,1) for bbb" in {
      testInstance.twosAndThreeMatches("bbb") mustBe (0,1)
    }

    "return (1,1) for aabbb" in {
      testInstance.twosAndThreeMatches("aabbb") mustBe (1,1)
    }

    "return (0,1) for aaccbbbddd" in {
      testInstance.twosAndThreeMatches("aaccbbbddd") mustBe (1,1)
    }

    "return (0,0) for empty string" in {
      testInstance.twosAndThreeMatches("") mustBe (0,0)
    }
  }

  "checksum" should {
    "return 0 for aa" in {
      testInstance.checksum(Seq("aa")) mustBe 0
    }

    "return 0 for bbb" in {
      testInstance.checksum(Seq("bbb")) mustBe 0
    }

    "return 1 for aabbb" in {
      testInstance.checksum(Seq("aabbb")) mustBe 1
    }

    "return 6 for aabbb,aa,bbbdd" in {
      testInstance.checksum(Seq("aabbb","aa","bbbdd")) mustBe 6
    }

    "return 6888 for day02 input" in {
      val ids = readLines("2018/day02/input.txt")

      testInstance.checksum(ids) mustBe 6888
    }
  }

  "firstSimilar" should {
    "match aaa,aba" in {
      val ids = Seq("aaa","bbb","aba")

      testInstance.firstSimilar(ids) mustBe Some("aa")
    }

    "match None" in {
      val ids = Seq("aaa","bbb","abc")

      testInstance.firstSimilar(ids) mustBe None
    }

    "match ids" in {
      val ids = readLines("2018/day02/input.txt")

      testInstance.firstSimilar(ids) mustBe Some("icxjvbrobtunlelzpdmfkahgs")
    }
  }
}
