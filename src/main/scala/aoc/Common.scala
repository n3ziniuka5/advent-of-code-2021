package aoc

object Common {
  def timed[A](label: String, f: => A): Unit = {
    val start = System.currentTimeMillis()
    val res   = f
    val end   = System.currentTimeMillis()

    println(s"$label answer - $res It took ${end - start}ms")
  }
}
