package cybersaurus.adventofcode

import scala.io.Source

object Input {
  def readLines(source: String): Stream[String] =
    Source
      .fromResource(source)
      .getLines
      .toStream
}
