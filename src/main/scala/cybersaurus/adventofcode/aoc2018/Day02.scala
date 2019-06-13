package cybersaurus.adventofcode.aoc2018

class Day02 {

  //
  // Part 1
  //

  implicit class Tuple2Ops[A](tuple: (A,A)) {
    def mapTuple2[B](f: A => B): (B,B) =
      (
        f(tuple._1),
        f(tuple._2)
      )

    def combine[B](other: (A,A))(f: (A,A) => B): (B,B) =
      (
        f(tuple._1,other._1),
        f(tuple._2,other._2)
      )
  }

  implicit class BooleanOps(b: Boolean) {
    def asInt: Int = if (b) 1 else 0
  }


  def checksum(ids: Seq[String]): Int = {
    val (twos, threes) =
      ids.map { twosAndThreeMatches }
         .foldLeft((0,0)) { case (acc, e) => e.combine(acc){_ + _} }

    twos * threes
  }

  private[aoc2018] def twosAndThreeMatches(id: String): (Int, Int) =
    id.groupBy{ ch => ch }
      .mapValues { _.size }
      .filter { case (_,count) => Set(2,3).contains(count) }
      .partition { case (_, count) => count == 2 }
      .mapTuple2 { m => if (m.nonEmpty) 1 else 0 }

  //
  // Part 2
  //

  implicit class TraversableOps[A](t: Traversable[A]) {
    def allPairs: Traversable[(A,A)] = {
      for {
        a <- t
        b <- t.dropWhile(_ != a).drop(1)
      } yield (a -> b)
    }
  }

  private[aoc2018] def commonChars(s1: String, s2: String): String =
    s1.zip(s2)
      .collect{ case (char1,char2) if (char1 == char2) => char1 }
      .mkString

  def firstSimilar(ids: Traversable[String], tolerance: Int = 1): Option[String] =
    ids
      .toStream
      .allPairs
      .map{ case (s1,s2) => (s1,s2,commonChars(s1,s2)) }
      .collectFirst{ case (s1,s2,common) if (s1.length - common.length <= tolerance) => common }
}
