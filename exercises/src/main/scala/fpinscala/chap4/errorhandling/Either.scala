package fpinscala.chap4.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(value) => Right(f(value))
   case left @ Left(_) => left
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(value) => f(value)
   case left @ Left(_) => left
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case right @ Right(_) => right
   case _ => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   flatMap(va => b.map(vb => f(va, vb)))
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object EitherRunner {
  def main(args: Array[String]): Unit = {
    val eitherError: Either[String, Int] = Left("some error")
    println(Right(21).map(_ * 2))
    println(eitherError.map(_ * 2))
    println(Right(21).flatMap(_ => Left("oops")))
    println(eitherError.orElse(Right(42)))
    println(Right(21).orElse(Right(42)))
    println(Right(21).map2(Right(21))(_ + _))
    println(eitherError.map2(Right(21))(_ + _))
    println(Right(21).map2(eitherError)(_ + _))
  }
}