package fpinscala.chap2.gettingstarted

import scala.annotation.tailrec

object GettingStarted {
  def main(args: Array[String]): Unit = {
    println(s"EXERCISE 1")
    println(s"Expected: 0, 1, 1, 2, 3, 5, 8")
    println(s"Actual:   ${(for(x <- 0 to 6) yield fib(x)).mkString(", ")}")

    println(s"EXERCISE 2")
    val gt = (a: Int, b: Int) => a > b
    println(s"isSorted([1]): ${isSorted(Array(1), gt)}")
    println(s"isSorted([1, 2, 3]): ${isSorted(Array(1, 2, 3), gt)}")
    println(s"isSorted([1, 3, 2]): ${isSorted(Array(1, 3, 2), gt)}")

    println(s"EXERCISE 3 & 4")
    val add = (a: Int, b: Int) => a + b
    val curriedAdd = curry(add)
    val add1 = curriedAdd(1)
    println(s"curry: (Expected, Result) => (3, ${add1(2)})")
    val uncurriedAdd = uncurry(curriedAdd)
    println(s"uncurry: (Expected, Result) => (3, ${add(1, 2)})")

    println(s"EXERCISE 5")
    println("No proof needed.")
  }

  // Exercise 1: Write a function to compute the nth fibonacci number
  def fib(n: Int): Int = {
    @tailrec
    def iter(n: Int, a: Int, b: Int): Int = {
      if(n <= 0) a
      else iter(n - 1, b, a + b)
    }

    iter(n, 0, 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def iter(i: Int): Boolean = {
      if(i == as.length - 1) true
      else
        if(gt(as(i + 1), as(i))) iter(i + 1)
        else false
    }

    iter(0)
  }

  // Exercise 3: Implement `curry`.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 4: Implement `uncurry`
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 5: Implement `compose`
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))
}