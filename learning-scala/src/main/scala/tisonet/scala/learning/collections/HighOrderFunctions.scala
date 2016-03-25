package tisonet.scala.learning.collections

object HighOrderFunctions {

    def map[A, B](list: List[A], fn: (A => B)) : List[B] = {
        list match {
            case List() => Nil
            case head :: tail => fn (head) :: map(tail, fn)
        }
    }

    def map2[A, B](list: List[A], fn: (A => B)) : List[B] = {
        for(x <- list) yield fn(x)
    }

    def flatten[B] (xss: List[List[B]]) : List[B] = xss match {
        case List() => Nil
        case head :: tail => head ::: flatten(tail)
    }

    def flatMap[A, B] (xs: List[A]) (f: A => List[B]) : List[B] = {
        flatten (map(xs, f))
    }
}
