import scala.annotation.tailrec

object session {

  // Not tail recursive:
  def badFactorial(n: Int): Int = {
    if (n == 0) 1 else n * badFactorial(n - 1)
  }

  def factorial(n: Int): Int = {
    @tailrec
    def loop(acc: Int, i: Int): Int = {
      if (i == 0) acc
      else loop(acc * i, i - 1)
    }
    loop(1, n)
  }

  factorial(5)
  badFactorial(5)
}
