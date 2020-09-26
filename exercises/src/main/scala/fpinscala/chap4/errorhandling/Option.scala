package fpinscala.chap4.errorhandling


import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(x => Some(x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap { x =>
    if(f(x)) Some(x)
    else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(va => b.map(vb => f(va, vb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List.empty): Option[List[A]])((x, xs) => map2(x, xs)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List.empty): Option[List[B]])((x, xs) => map2(f(x), xs)(_ :: _))

  def sequenceFromTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}

object OptionRunner {
  def main(args: Array[String]): Unit = {
    def onlyEven(x: Int): Option[Int] = if(x % 2 == 0) Some(x) else None
    val noneInt: Option[Int] = None
    println(Some(1).map(_ * 10))
    println(noneInt.map(_ * 10))
    println(Some(1).getOrElse(0))
    println(noneInt.getOrElse(0))
    println(Some(1).flatMap(_ => Some(42)))
    println(noneInt.flatMap(_ => None))
    println(Some(1).orElse(Some(2)))
    println(noneInt.orElse(Some(2)))
    println(Some(1).filter(x => x > 0))
    println(Some(1).filter(x => x < 0))
    println(noneInt.filter(x => x > 0))
    println(Option.mean(List(2, 4, 6)))
    println(Option.variance(List(2, 4, 6)))
    println(Option.sequence(List(None, None)))
    println(Option.sequence(List(Some(1), Some(2))))
    println(Option.sequence(List(Some(1), None, Some(2))))
    println(Option.traverse(List(1, 2))(onlyEven))
    println(Option.traverse(List(2, 4))(onlyEven))
  }
}