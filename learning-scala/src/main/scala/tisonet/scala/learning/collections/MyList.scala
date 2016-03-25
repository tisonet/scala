package tisonet.scala.learning.collections

// List Data type
sealed trait MyList[+A]

// List data constructor representing empty list
case object MyNil extends MyList[Nothing]

// A list data constructor representing non-empty list
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  // Function sum int values, uses pattern matching
  def sum(ints: MyList[Int]): Int = ints match {
      case MyNil => 0
      case MyCons(x,xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
      case MyNil => 1.0
      case MyCons(0.0, _) => 0.0
      case MyCons(x,xs) => x * product(xs)
  }

  // Variadic function (A*) takes zero or more arguments of type A
  // apply is called implicitly val ex = List(1,2,3)
  def apply[A] (as: A*): MyList[A] =
      if(as.isEmpty) MyNil
      else MyCons(as.head, apply(as.tail: _*))

  def tail[A](l: MyList[A]): MyList[A] = l match {
      case MyNil => sys.error("tail of empty list")
      case MyCons(_, xs) => xs
  }

  def setHead[A] (head: A, l: MyList[A]) = l match {
      case MyNil => sys.error("setHead on empty list")
      case MyCons(_, xs) => MyCons(head, xs)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] =
    if (n <= 0) l
    else l match {
      case MyNil => MyNil
      case MyCons(_,t) => drop(t, n-1)
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case MyCons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case MyNil => a2
      case MyCons(h,t) => MyCons(h, append(t, a2))
    }

}