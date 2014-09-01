package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(a) => a
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }

  def fold[A, B](t: Tree[A], f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l, f)(g), fold(r, f)(g))
    }

  def sizeByFold[A](t: Tree[A]): Int =
    fold[A, Int](t, _ => 1)(_ + _)

  def maximumByFold(t: Tree[Int]): Int =
    fold[Int, Int](t, n => n)(_ max _)

  def depthByFold[A](t: Tree[A]): Int =
    fold[A, Int](t, _ => 1)((l, r) => 1 + (l max r))

  def mapByFold[A, B](t: Tree[A], f: A => B): Tree[B] =
    fold[A, Tree[B]](t, v => Leaf(f(v)))(Branch(_, _))
}