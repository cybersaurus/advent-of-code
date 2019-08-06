package cybersaurus.adventofcode.aoc2018.day05

trait Day05 {
  def reactions(polymer: String): String = {
    def reacted(a: Char, b: Char): Boolean = Math.abs(a.toInt - b.toInt) == 32

    polymer.foldLeft("") {
      case ("", el)   => el.toString
      case (acc, el)  => if (reacted(acc.last, el)) acc.init else acc :+ el
    }
  }
}
