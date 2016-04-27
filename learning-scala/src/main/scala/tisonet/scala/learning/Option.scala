package tisonet.scala.learning

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
        this match {
            case Some(value) => Some(f(value))
            case None => None
        }
    }

    def flatMap[B](f: A => Option[B]) = {
        map(f).getOrElse(None)
    }

    def getOrElse[B >: A](default: B): B = {
        this match {
            case Some(value) => value
            case None => default
        }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        map(Some(_)) getOrElse ob
    }

    def filter(f: A => Boolean): Option[A] = {
        flatMap(a => if (f(a)) Some(a) else None)
    }

    def map2[B,C](b: Option[B]) (op: (A, B) => C): Option[C] = {
        flatMap(aa => b.map(bb => op(aa, bb)))
    }
}

case class Some[+A](value: A) extends Option[A]

case object None extends Option[Nothing]

