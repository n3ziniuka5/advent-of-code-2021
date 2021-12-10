package day1

import java.math.MathContext
import scala.io.Source
import scala.math.BigDecimal.RoundingMode

object Day7 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day7.txt").getLines().toList.head.split(",").map(_.toInt).toList
    println(s"Part 1 - ${part1(lines)}")
    println(s"Part 2 - ${part2(lines)}")
  }

  def part1(positions: List[Int]): Int = {
    val sorted        = positions.sorted
    val positionCount = positions.size
    val medianValues = if (positions.size % 2 == 0) {
      List(sorted(positionCount / 2 - 1), sorted(positionCount / 2))
    } else {
      List(sorted(positionCount / 2))
    }

    medianValues.distinct.map { target =>
      positions.map(p => Math.abs(target - p)).sum
    }.min
  }

  def part2(positions: List[Int]): Int = {
    val averages = {
      val trueAverage = BigDecimal(positions.sum) / positions.size
      List(trueAverage.setScale(0, RoundingMode.DOWN).intValue, trueAverage.setScale(0, RoundingMode.UP).intValue)
    }

    averages.map { target =>
      positions.map { p =>
        val dist = Math.abs(target - p)
        (1 + dist) * dist / 2
      }.sum
    }.min
  }
}
