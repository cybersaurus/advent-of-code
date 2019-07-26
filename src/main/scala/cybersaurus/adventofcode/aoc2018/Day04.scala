package cybersaurus.adventofcode.aoc2018

import java.time.LocalDateTime
import java.time.LocalDateTime.parse
import java.time.format.DateTimeFormatter

import scala.collection.immutable.Map
import scala.collection.mutable.Buffer


sealed trait GuardEvent {
  val at: LocalDateTime
}
object GuardEvent {
  private val dateTimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  private val shiftStarts = """^\[([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})\] Guard #([0-9]{1,4}) begins shift$""".r
  private val fallsAsleep = """^\[([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})\] falls asleep$""".r
  private val wakesUp     = """^\[([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2})\] wakes up$""".r


  def apply(event: String): GuardEvent = {
    def parseDateTime(dateTime: String): LocalDateTime = parse(dateTime, dateTimeFormat)

    event match {
      case shiftStarts(at, guardId) => StartsShift(parseDateTime(at), guardId.toInt)
      case fallsAsleep(at)          => FallsAsleep(parseDateTime(at))
      case wakesUp(at)              => WakesUp(parseDateTime(at))
    }
  }

  def parseEvents(events: Seq[String]): Seq[GuardEvent] = events map apply
}
case class StartsShift(override val at: LocalDateTime, guardId: Int) extends GuardEvent
case class FallsAsleep(override val at: LocalDateTime) extends GuardEvent
case class WakesUp(override val at: LocalDateTime) extends GuardEvent

case class SleepPeriod(start: LocalDateTime, end: LocalDateTime) {
  lazy val minutes: Seq[Int] = start.getMinute until end.getMinute
  lazy val length: Int = minutes.size
}
object SleepPeriod {
  def createSleepPeriods(events: Seq[GuardEvent]): Seq[SleepPeriod] =
    events
      .grouped(2)
      .collect { case Seq(fallsAsleep: FallsAsleep, wakesUp: WakesUp) => SleepPeriod(fallsAsleep.at,wakesUp.at) }
      .toList
}

trait Day04 {
  def groupedByStartsShift(events: Seq[GuardEvent]): Seq[(StartsShift, Seq[SleepPeriod])] = {
    val empty = (Seq.empty[(StartsShift, Buffer[GuardEvent])])

    events
      .sorted(Day04.guardEventOrdering)
      .foldLeft(empty) { case (acc,ge) =>
        ge match {
          case ss: StartsShift => acc :+ (ss, Buffer.empty[GuardEvent])
          case  _: FallsAsleep |
                _: WakesUp     => acc.last._2 += ge; acc
        }
      }
      .map { case (ss, ges) => (ss, SleepPeriod.createSleepPeriods(ges)) }
  }

  // Part 1
  def sleepPeriodsByGuardId(events: Seq[(StartsShift, Seq[SleepPeriod])]): Map[Int, Seq[SleepPeriod]] = {
    events
      .groupBy { case (ss,_) => ss.guardId }
      .mapValues { seq => seq.flatMap { case (_,sps) => sps } }
  }

  def guardAsleepMost(sleepPeriodsByGuard: Map[Int, Seq[SleepPeriod]]): Int =
    sleepPeriodsByGuard
      .maxBy { case (_,sps) => sps.map {_.length}.sum }
      ._1

  def minuteAsleepMost(sleepPeriods: Seq[SleepPeriod]): Int =
    sleepPeriods
      .flatMap{_.minutes}
      .groupBy(identity)
      .maxBy{ case (_, occurrences) => occurrences.size }
      ._1

  // Part 2
  def mostFrequentMinuteByGuardId(sleepPeriodsByGuard: Map[Int, Seq[SleepPeriod]]): Map[Int, (Int,Int)] = {
    sleepPeriodsByGuard
      .filterNot { case (guardId, events) => events.isEmpty }
      .mapValues { sleepPeriods =>
        sleepPeriods
          .flatMap {_.minutes}
          .groupBy {identity}
          .map { case (min, occurrences) => (min, occurrences.size) }
          .maxBy { case (_, count) => count }
      }
  }

  def guardAndMostFrequentMinute(minuteAndOccurrencesByGuardId: Map[Int, (Int, Int)]): (Int, Int) = {
    val (guardId, (min,count)): (Int, (Int,Int)) = minuteAndOccurrencesByGuardId
      .maxBy { case (guardId, (min,count)) => count }

    (guardId, min)
  }
}

/*
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
 */
object Day04 {
  implicit val guardEventOrdering = new Ordering[GuardEvent] {
    override def compare(x: GuardEvent, y: GuardEvent): Int = x.at compareTo y.at
  }
}