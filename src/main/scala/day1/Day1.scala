package day1

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1.txt").getLines().toList
    println(s"Part 1 - ${part1(lines)}")
    println(s"Part 2 - ${part2(lines)}")
  }

  def part1(lines: List[String]): Int = {
    lines.map(_.toInt)
      .sliding(2)
      .count { l =>
        l(1) > l(0)
      }
  }

  def part2(lines: List[String]): Int = {
    lines.map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count { l =>
        l(1) > l(0)
      }
  }
}
