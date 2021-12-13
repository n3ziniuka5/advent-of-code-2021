package aoc

import aoc.Common.timed
import zio.prelude.{MultiSet, ZSet}

import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  case class Point(x: Int, y: Int)
  case class Line(from: Point, to: Point)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day5.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def parseInput(lines: List[String]): List[Line] = {
    lines.map { lineString =>
      val Array(from, to) = lineString.split("->").map(_.trim).map { pointString =>
        val Array(x, y) = pointString.split(',').map(_.toInt)
        Point(x, y)
      }
      Line(from, to)
    }
  }

  @tailrec
  def overlaps(lines: List[Line], pointCounts: MultiSet[Point] = MultiSet.empty): Int = {
    lines match {
      case head :: tail =>
        val points = if (head.from.x == head.to.x || head.from.y == head.to.y) {
          for {
            x <- 0 to Math.abs(head.from.x - head.to.x)
            y <- 0 to Math.abs(head.from.y - head.to.y)
          } yield Point(Math.min(head.from.x, head.to.x) + x, Math.min(head.from.y, head.to.y) + y)
        } else {
          for {
            x <- Math.min(head.from.x, head.to.x) to Math.max(head.from.x, head.to.x)
            y <- Math.min(head.from.y, head.to.y) to Math.max(head.from.y, head.to.y)
            if Math.abs(head.from.x - x) == Math.abs(head.from.y - y)
          } yield Point(x, y)
        }

        overlaps(tail, pointCounts <> MultiSet.fromIterable(points))
      case Nil => pointCounts.toMap.count(_._2 > 1)
    }
  }

  def part1(lines: List[String]): Int = {
    val input = parseInput(lines)
    val straightLines = input.filter { line =>
      line.from.x == line.to.x || line.from.y == line.to.y
    }
    overlaps(straightLines)
  }

  def part2(lines: List[String]): Int = {
    overlaps(parseInput(lines))
  }
}
