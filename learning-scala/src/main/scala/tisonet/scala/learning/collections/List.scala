package tisonet.scala.learning.collections

// List Data type
sealed trait List[+A]

// List data constructor representing empty list
case object Nil extends List[Nothing]

// A list data constructor representing non-empty list
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  // Function sum int values, uses pattern matching
  def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
  }

  // Variadic function (A*) takes zero or more arguments of type A
  // apply is called implicitly val ex = List(1,2,3)
  def apply[A] (as: A*): List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
  }

  def setHead[A] (head: A, l: List[A]) = l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, xs) => Cons(head, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }


}