package tisonet.scala.learning.collections

import tisonet.scala.learning.collections

/***
  * Represents (non-strict) lazy list, values are evaluated on demand.
  */
trait Stream[+A] {

    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h()) // triggers thunk evaluation
    }

    def toList: List[A] = {

        def loop (s: Stream[A], l: List[A]) : List[A] = s match {
            case Empty => l.reverse
            case Cons(h, t) => loop(t(), h() :: l)
        }

        loop(this, List())
    }

    def take(n: Int): Stream[A] = {

        def loop (s: Stream[A], n: Int) : Stream[A] = s match {
            case Empty => Empty
            case Cons(h, t) if n > 0 => Cons(h, () => loop(t(), n - 1))
            case _ if n == 0 => Empty
        }

        loop(this, n)
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match  {
        case Cons(h,t) if f(h()) => Stream.cons(h(), t() takeWhile f)
        case _ => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def exists(p: A => Boolean): Boolean = {
        foldRight(false) ((a,b) => p(a) || b)
    }

    def forAll(p: A => Boolean): Boolean = {
        foldRight(true) ((a,b) => p(a) && b)
    }

    def takeWhile_2(f: A => Boolean): Stream[A] = {
        foldRight(Stream.empty[A]) ((a, b) => {
            if (f(a)) Stream.cons(a, b)
            else Stream.empty
        })
    }

}
case object Empty extends Stream[Nothing]
// head and tail are both non-strict, lazy
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

    // Smart constructor
    // Thunk values are evaluated more times. Ideally we would like evaluates once and cache value,
    // for that we can create "smart constructor" which helps here:
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd // once evaluated then value is cached
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
        if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }

}