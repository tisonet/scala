package tisonet.scala.learning

sealed trait Either [+E, +A] {
    def map[B] (f: A => B): Either[E, B] = this match {
        case Right(a) => Right(f(a))
        case l@Left(e) => l
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Right(a) => f(a)
        case l@Left(e) => l
    }

    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => b
        case r@Right(a) => r
    }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
    def Try[A](a: => A): Either[Exception, A] = {
        try Right(a)
        catch { case e: Exception => Left(e)}
    }
}


