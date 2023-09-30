import scala.annotation.experimental

@experimental
@cached
def sumTo(n: Int): Int =
  println("sumTo")
  (1 to n).sum
@experimental
@main
def main(): Unit =
  println(sumTo(100))
  println(sumTo(100))