import scala.annotation.tailrec

// Exercise 2.1
def fib(n: Int): Int = {
  @tailrec
  def loop(i: Int, n_2: Int, n_1: Int): Int =
    if (i == n) n_2 + n_1
    else loop(i + 1, n_1, n_2 + n_1)

  n match {
    case 0 => 0
    case 1 => 1
    case _ => loop(2, 0, 1)
  }
}

