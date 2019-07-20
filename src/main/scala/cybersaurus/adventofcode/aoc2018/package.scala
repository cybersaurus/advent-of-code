package cybersaurus.adventofcode

import scala.io.Source

package object aoc2018 {
  def readLines(source: String): Stream[String] =
    Source
      .fromResource(source)
      .getLines
      .toStream
}
