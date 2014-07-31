package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, this.tail(l))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(this.tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else t
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((b, a) => a + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil) {
    (result, x) => Cons(x, result)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft2(xs, f(z, x))(f)
    }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z) {
    (b, a) => f(a, b)
  }

  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2) {
      (z, result) => Cons(z, result)
    }

  def flatten[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil: List[A]) {
      (resultList, a) => appendWithFold(resultList, a)
    }

  def mapPlus1(as: List[Int]): List[Int] = foldRight2(as, Nil: List[Int]) {
    (x, result) => Cons(x + 1, result)
  }

  def mapDouble2String(as: List[Double]): List[String] = foldRight2(as, Nil: List[String]) {
    (x, result) => Cons(x.toString, result)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil: List[B]) {
    (x, result) => Cons(f(x), result)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight2(l, Nil: List[A]) {
    (x, result) => f(x) match {
      case true => Cons(x, result)
      case false => result
    }
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight2(l, Nil: List[B]) {
    (x, result) => appendWithFold(f(x), result)
  }

  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => f(x) match {
      case true => List(x)
      case false => Nil
    })

  def zipPlus(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (Cons(hl, zl), Cons(hr, zr)) => Cons(hl + hr, zipPlus(zl, zr))
      case _ => Nil
    }
  }

  def zipWith[A, B](l: List[A], r: List[A], f: (A, A) => B): List[B] = {
    def internalZip(li: List[A], ri: List[A], result: List[B]): List[B] = {
      (li, ri) match {
        case (Cons(hli, zli), Cons(hri, zri)) => internalZip(zli, zri, Cons(f(hli, hri), result))
        case _ => result
      }
    }

    List.reverse(internalZip(l, r, Nil))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    if (List.length(l) < List.length(sub)) false
    else
      foldLeft(zipWith[A, Boolean](l, sub, _ == _), true)(_ && _) match {
        case true => true
        case false => hasSubsequence(List.tail(l), sub)
      }

}
