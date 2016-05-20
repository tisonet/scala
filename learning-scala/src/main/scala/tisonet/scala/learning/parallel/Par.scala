package tisonet.scala.learning.parallel

trait Par[A] {}

object Par {
    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    def sum(ints: IndexedSeq[Int]): Par[Int] = {
        if(ints.size <= 1)
            Par.unit(ints.headOption getOrElse 0)
        else {
            val (l,r) = ints.splitAt(ints.length/2)
            Par.map2(sum(l), sum(r))(_ + _)
        }
    }

    def map2[A](parA: => Par[A], parB: => Par[A])(f: (A, A) => A): Par[A]  = {


    }
}
