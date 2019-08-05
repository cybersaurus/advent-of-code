package cybersaurus.adventofcode.aoc2018.day01

import cybersaurus.adventofcode.Input.readLines

import scala.annotation.tailrec

trait Day01 {

  def firstRepeat(cs: Seq[Int]): Int

  protected final def infiniteStreamOf(cs: Seq[Int]): Stream[Int] =
    Stream.continually(cs.toStream).flatten
}

object Day01 {
  def changes(source: String): Seq[Int] = readLines(source).map{_.toInt}

  def frequency(cs: Seq[Int]): Int = cs.sum
}

class RecursiveDay01 extends Day01 {

  override def firstRepeat(cs: Seq[Int]): Int = {

    @tailrec
    def _firstRepeat(remaining: Seq[Int], acc: Seq[Int]): Int = {

      val sum = acc.head + remaining.head

      if (acc.contains(sum)) sum
      else {
        _firstRepeat(remaining.tail, sum +: acc)
      }
    }

    _firstRepeat(infiniteStreamOf(cs), Seq(0))
  }
}

class FoldLeftSlowDay01 extends Day01 {

  override def firstRepeat(cs: Seq[Int]): Int = {

    infiniteStreamOf(cs).foldLeft(Seq(0)) { case (acc, c) =>
      val sum = acc.last + c

      if (acc.contains(sum)) return sum
      else acc :+ sum
    }

    throw new IllegalStateException("Shouldn't get here!")
  }
}

class FoldLeftFastDay01 extends Day01 {

  override def firstRepeat(cs: Seq[Int]): Int = {

    infiniteStreamOf(cs).foldLeft((Set(0),0)) { case ((sums,lastSum), c) =>
      val sum = lastSum + c

      if (sums.contains(sum)) return sum
      else (sums + sum, sum)
    }

    throw new IllegalStateException("Shouldn't get here!")
  }
}

class ScanLeftDay01 extends Day01 {

  override def firstRepeat(cs: Seq[Int]): Int = {

    val empty = (Set.empty[Int], 0)

    infiniteStreamOf(cs)
      .scanLeft(empty){ case ((prevSums,lastSum), c) =>
        val newSum = lastSum + c

        (prevSums + lastSum, newSum)
      }
      .find { case (prevSums, lastSum) => prevSums.contains(lastSum) }
      .map  { case (prevSums, lastSum) => lastSum }
      .get
  }
}