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

assert((0 to 8).map(fib) == Seq(0, 1, 1, 2, 3, 5, 8, 13, 21))

// Exercise 2.2
@tailrec
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  as match {
    case Array(a, b, _*) => ordered(a, b) && isSorted(as.tail, ordered)
    case _ => true
  }
}

assert(isSorted[Int](Array.empty, (a, b) => a < b))
assert(isSorted[Int](Array(1, 2, 3), (a, b) => a < b))
assert(isSorted[Int](Array(3, 2, 1), (a, b) => a < b))