package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def toListRecursively: List[A] = this match {
    case Cons(l, r) => l() :: r().toListRecursively
    case Empty => Nil
  }

  def toListUsingFold: List[A] = foldRight(List[A]())((a, b) => a :: b)

  def toListTailRec: List[A] = {
    @tailrec
    def internal(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => internal(t(), h() :: acc)
      case Empty => acc
    }
    internal(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case _ => Empty
    }

  def takeUsingUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), m) if m > 0 => Some(h(), (t(), m - 1))
    case _ => None
  }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Cons(h, t) => t().drop(n - 1)
      case _ => Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // EXERCISE5の俺の解答。しかしnon-strictじゃない。
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) {
      case (a, s) if p(a) => cons(a, s)
      case _ => Stream.empty[A]
    }

  // EXERCISE5の本家の解答。non-strict。何が違うんだ。
  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else empty)

  def takeWhileUsingUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some(h(), t())
    case _ => None
  }

  def forAllUsingFoldRight(p: A => Boolean): Boolean = foldRight(true)((a, result) => p(a) && result)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def headOptionUsingFoldRight: Option[A] = foldRight[Option[A]](None) {
    (h, t) => Some(h)
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapUsingUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>
    if (f(h)) cons(h, t)
    else t
  )

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  def zipWith[B >: A, C](s: Stream[B], f: (B, B) => C): Stream[C] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = (this, s2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => cons((Some(h1()), Some(h2())), t1().zipAll(t2()))
    case (Empty, Cons(h2, t2)) => cons((None, Some(h2())), empty.zipAll(t2()))
    case (Cons(h1, t1), Empty) => cons((Some(h1()), None), t1().zipAll(empty))
    case (_, _) => empty
  }

  // これだとsignatureが違う。型が違う場合に期待しない結果になることがある気がする。
  def startsWithIncorrect[B >: A](s: Stream[B]): Boolean = zipWith[B, Boolean](s, _ == _).foldRight(true)((v, bool) => v && bool)

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile(v => v._1.isDefined && v._2.isDefined)
      .foldRight(true)((v, bool) => v._1.get == v._2.get && bool)

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h,t) => Some((Cons(h,t), t()))
    case _ => None
  } append Stream(empty)

  def scanRight[B](base: B)(f: (A, => B) => B): Stream[B] = this match {
    case Cons(h, t) => cons(foldRight(base)(f), t().scanRight(base)(f))
    case _ => Stream(base)
  }

  def scanRightUsingTails[B >: A](base: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(base)(f))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(b => Some(b, b))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(m => Some(m, m + 1))

  def fibs: Stream[Int] = {
    def go(prev: Int, next: Int): Stream[Int] = cons(prev, go(next, prev + next))

    go(0, 1)
  }

  def fibsUsingUnfold: Stream[Int] = unfold(List(0, 1))(list => Some(list(0), List(list(1), list.sum)))

  def fibsUsingUnfoldWithTuple: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }
}