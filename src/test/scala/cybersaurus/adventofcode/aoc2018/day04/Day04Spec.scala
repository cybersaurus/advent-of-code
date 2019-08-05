package cybersaurus.adventofcode.aoc2018.day04

import java.time.LocalDateTime

import cybersaurus.adventofcode.Input.readLines
import org.scalatest.{Matchers, WordSpec}


class Day04Spec extends WordSpec with Matchers {

  "GuardEvent" when {

    "apply" should {
      "parse StartsShift" in {
        // [1518-11-05 00:03] Guard #99 begins shift
        GuardEvent("[1518-11-05 00:03] Guard #99 begins shift") shouldBe StartsShift(LocalDateTime.of(1518, 11, 5, 0, 3), 99)
      }

      "parse FallsAsleep" in {
        // [1518-11-05 00:45] falls asleep
        GuardEvent("[1518-11-05 00:45] falls asleep") shouldBe FallsAsleep(LocalDateTime.of(1518, 11, 5, 0, 45))
      }

      "parse WakesUp" in {
        // [1518-11-05 00:55] wakes up
        GuardEvent("[1518-11-05 00:55] wakes up") shouldBe WakesUp(LocalDateTime.of(1518, 11, 5, 0, 55))
      }
    }

    "ordering" should {
      import Day04.guardEventOrdering

      val midnight: LocalDateTime = LocalDateTime.of(1518, 11, 5, 0, 0)

      val startsShiftAt = midnight.plusMinutes(1)
      val fallsAsleepAt = midnight.plusMinutes(20)
      val wakesUpAt = midnight.plusMinutes(30)

      "sort GuardEvents" in {
        val events: Seq[GuardEvent] = Seq(
          WakesUp(wakesUpAt),
          FallsAsleep(fallsAsleepAt),
          StartsShift(startsShiftAt, 123),
        )

        events.sorted should contain theSameElementsInOrderAs (Seq(
          StartsShift(startsShiftAt, 123),
          FallsAsleep(fallsAsleepAt),
          WakesUp(wakesUpAt),
        ))
      }
    }
  }

  "Day04" when {
    val testInstance = new Day04 {}

    val guard1 = 123
    val guard2 = 123456

    val midnight: LocalDateTime = LocalDateTime.of(1518,11,5,0,0)

    val startsShiftAt = midnight.plusMinutes(1)
    val fallsAsleepAt = midnight.plusMinutes(5)
    val wakesUpAt     = midnight.plusMinutes(10)

    val sleepEvents: Seq[GuardEvent] = Seq(
      WakesUp(wakesUpAt),
      FallsAsleep(fallsAsleepAt),
      WakesUp(wakesUpAt.plusHours(2)),
      FallsAsleep(fallsAsleepAt.plusHours(2)),
      WakesUp(wakesUpAt.plusHours(1)),
      FallsAsleep(fallsAsleepAt.plusHours(1)),
    )
    val shiftEvents: Seq[GuardEvent] = sleepEvents :+ StartsShift(startsShiftAt, guard1)

    val nextDayStartsShiftAt = midnight.plusDays(1).plusMinutes(1)
    val nextDayFallsAsleepAt = midnight.plusDays(1).plusMinutes(20)
    val nextDayWakesUpAt     = midnight.plusDays(1).plusMinutes(30)

    val eventsNextShift: Seq[GuardEvent] = Seq(
      WakesUp(nextDayWakesUpAt),
      FallsAsleep(nextDayFallsAsleepAt),
      WakesUp(nextDayWakesUpAt.plusHours(2)),
      FallsAsleep(nextDayFallsAsleepAt.plusHours(2)),
      WakesUp(nextDayWakesUpAt.plusHours(1)),
      FallsAsleep(nextDayFallsAsleepAt.plusHours(1)),
    ) :+ StartsShift(nextDayStartsShiftAt, guard2)


    "groupedByStartsShift" should {

      "group single sleep period for single shift" in {
        val events: Seq[GuardEvent] = Seq(
          WakesUp(wakesUpAt),
          FallsAsleep(fallsAsleepAt),
        ) :+ StartsShift(startsShiftAt, guard1)

        val grouped: Seq[(StartsShift, Seq[SleepPeriod])] = testInstance.groupedByStartsShift(events)

        grouped should contain theSameElementsInOrderAs Seq(
          StartsShift(startsShiftAt, guard1) -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
          )
        )
      }

      "group multiple sleep periods for single shift" in {
        val grouped: Seq[(StartsShift, Seq[SleepPeriod])] = testInstance.groupedByStartsShift(shiftEvents)

        grouped should contain theSameElementsInOrderAs Seq(
          StartsShift(startsShiftAt, guard1) -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
            SleepPeriod(fallsAsleepAt.plusHours(1), wakesUpAt.plusHours(1)),
            SleepPeriod(fallsAsleepAt.plusHours(2), wakesUpAt.plusHours(2)),
          )
        )
      }

      "group multiple sleep periods for multiple shifts" in {
        val grouped: Seq[(StartsShift, Seq[SleepPeriod])] = testInstance.groupedByStartsShift(eventsNextShift ++ shiftEvents)

        grouped should contain theSameElementsInOrderAs Seq(
          StartsShift(startsShiftAt, guard1) -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
            SleepPeriod(fallsAsleepAt.plusHours(1), wakesUpAt.plusHours(1)),
            SleepPeriod(fallsAsleepAt.plusHours(2), wakesUpAt.plusHours(2)),
          ),
          StartsShift(nextDayStartsShiftAt, guard2) -> Seq(
            SleepPeriod(nextDayFallsAsleepAt, nextDayWakesUpAt),
            SleepPeriod(nextDayFallsAsleepAt.plusHours(1), nextDayWakesUpAt.plusHours(1)),
            SleepPeriod(nextDayFallsAsleepAt.plusHours(2), nextDayWakesUpAt.plusHours(2)),
          )
        )
      }
    }

    "sleepPeriodsByGuardId" should {
      "group for a single guard id" in {

        val grouped: Seq[(StartsShift, Seq[SleepPeriod])] = Seq(
          StartsShift(startsShiftAt, guard1) -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
          )
        )

        val sleepPeriodsByGuardId: Map[Int, Seq[SleepPeriod]] = testInstance.sleepPeriodsByGuardId(grouped)

        sleepPeriodsByGuardId should contain theSameElementsAs Map(
          guard1 -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
          )
        )
      }

      "group multiple guards for multiple shifts" in {
        val grouped: Seq[(StartsShift, Seq[SleepPeriod])] = Seq(
          StartsShift(startsShiftAt, guard1) -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
            SleepPeriod(fallsAsleepAt.plusHours(1), wakesUpAt.plusHours(1)),
            SleepPeriod(fallsAsleepAt.plusHours(2), wakesUpAt.plusHours(2)),
          ),
          StartsShift(nextDayStartsShiftAt, guard2) -> Seq(
            SleepPeriod(nextDayFallsAsleepAt, nextDayWakesUpAt),
            SleepPeriod(nextDayFallsAsleepAt.plusHours(1), nextDayWakesUpAt.plusHours(1)),
            SleepPeriod(nextDayFallsAsleepAt.plusHours(2), nextDayWakesUpAt.plusHours(2)),
          )
        )

        val sleepPeriodsByGuardId: Map[Int, Seq[SleepPeriod]] = testInstance.sleepPeriodsByGuardId(grouped)

        sleepPeriodsByGuardId should contain theSameElementsAs Map(
          guard1 -> Seq(
            SleepPeriod(fallsAsleepAt, wakesUpAt),
            SleepPeriod(fallsAsleepAt.plusHours(1), wakesUpAt.plusHours(1)),
            SleepPeriod(fallsAsleepAt.plusHours(2), wakesUpAt.plusHours(2)),
          ),
          guard2 -> Seq(
            SleepPeriod(nextDayFallsAsleepAt, nextDayWakesUpAt),
            SleepPeriod(nextDayFallsAsleepAt.plusHours(1), nextDayWakesUpAt.plusHours(1)),
            SleepPeriod(nextDayFallsAsleepAt.plusHours(2), nextDayWakesUpAt.plusHours(2)),
          )
        )
      }
    }

    "guardAsleepMost" should {
      "return guard2" in {
        val guard1Sleeps = Seq((1,5), (10,20), (30,40))
        val guard2Sleeps = Seq((1,5), (10,25), (30,55))

        val sleepPeriodsByGuardId: Map[Int, Seq[SleepPeriod]] = Map(
          guard1 -> guard1Sleeps.map{ case (f,w) => SleepPeriod(midnight.plusMinutes(f), midnight.plusMinutes(w)) },
          guard2 -> guard2Sleeps.map{ case (f,w) => SleepPeriod(midnight.plusMinutes(f), midnight.plusMinutes(w)) },
        )

        testInstance.guardAsleepMost(sleepPeriodsByGuardId) shouldBe guard2
      }
    }

    "minuteAsleepMost" should {
      "return 7" in {
        val shift1Sleeps = Seq((7,9), (10,11), (30,31))
        val shift2Sleeps = Seq((6,8), (11,12), (32,34))
        val shift3Sleeps = Seq((7,9), (12,13), (34,40))

        val sleepPeriodsByGuardId: Seq[SleepPeriod] =
          (shift1Sleeps ++ shift2Sleeps ++ shift3Sleeps).map{ case (f,w) => SleepPeriod(midnight.plusMinutes(f), midnight.plusMinutes(w)) }

        testInstance.minuteAsleepMost(sleepPeriodsByGuardId) shouldBe 7
      }
    }

    "Part 1" should {
      val guardEvents: Seq[GuardEvent] = GuardEvent.parseEvents(readLines("2018/day04/input.txt"))
      val eventsByShift: Seq[(StartsShift, Seq[SleepPeriod])] = testInstance.groupedByStartsShift(guardEvents)
      val sleepPeriodsByGuardId: Map[Int, Seq[SleepPeriod]] = testInstance.sleepPeriodsByGuardId(eventsByShift)

      "guard who sleeps the most is guardId 1993" in {
        testInstance.guardAsleepMost(sleepPeriodsByGuardId) shouldBe 1993
      }

      "minute guardId 1993 sleeps that most is minute 36" in {
        testInstance.minuteAsleepMost(sleepPeriodsByGuardId(1993)) shouldBe 36
      }
    }

    "Part 2" should {
      val guardEvents: Seq[GuardEvent] = GuardEvent.parseEvents(readLines("2018/day04/input.txt"))
      val eventsByShift: Seq[(StartsShift, Seq[SleepPeriod])] = testInstance.groupedByStartsShift(guardEvents)
      val sleepPeriodsByGuardId: Map[Int, Seq[SleepPeriod]] = testInstance.sleepPeriodsByGuardId(eventsByShift)

      "guard and minute most frequently asleep is (2137,50)" in {
        val minuteAndCountByGuardId: Map[Int, (Int, Int)] = testInstance.mostFrequentMinuteByGuardId(sleepPeriodsByGuardId)

        testInstance.guardAndMostFrequentMinute(minuteAndCountByGuardId) shouldBe (2137,50)
      }
    }
  }
}
