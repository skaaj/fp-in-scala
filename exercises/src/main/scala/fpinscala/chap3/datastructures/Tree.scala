package fpinscala.chap3.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[T](t: Tree[T]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => max(left) max max(right)
  }

  def depth[T](t: Tree[T]): Int = {
    def iter(t: Tree[T], acc: Int): Int = t match {
      case Leaf(_) => acc
      case Branch(left, right) => iter(left, acc + 1) max iter(right, acc + 1)
    }
    iter(t, 0)
  }

  def map[T, U](t: Tree[T], f: T => U): Tree[U] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left, f), map(right, f))
  }

  def fold[T, U](t: Tree[T])(f: T => U)(g: (U, U) => U): U = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeFold[T](t: Tree[T]): Int = fold(t)(_ => 1)(_ + _)
  def maxFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)
  def depthFold[T](t: Tree[T]): Int = fold(t)(_ => 0)((left, right) => 1 + (left max right))
  def mapFold[T, U](t: Tree[T], f: T => U): Tree[U] =
    fold(t)(value => Leaf(f(value)): Tree[U])((left, right) => Branch(left, right))
}

object TreeTest {
  def main(args: Array[String]): Unit = {
    val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    println(Tree.size(tree))
    println(Tree.max(tree))
    println(Tree.depth(tree))
    println(Tree.map(tree, (x: Int) => x * 10))
    println(Tree.sizeFold(tree))
    println(Tree.maxFold(tree))
    println(Tree.depthFold(tree))
    println(Tree.mapFold(tree, (x: Int) => x * 10))
  }
}