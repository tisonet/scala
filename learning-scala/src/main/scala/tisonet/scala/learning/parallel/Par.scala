package tisonet.scala.learning.parallel

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit



object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: => A): Par[A] = (s: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A] (get: A) extends Future[A] {
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
    }


    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def sum(ints: IndexedSeq[Int]): Par[Int] = {
        if(ints.size <= 1)
            Par.unit(ints.headOption getOrElse 0)
        else {
            val (l,r) = ints.splitAt(ints.length/2)
            Par.map2(Par.fork(sum(l)),Par.fork(sum(r)))(_ + _)
        }
    }

    def map2[A, B, C](parA: => Par[A], parB: => Par[B])(f: (A, B) => C): Par[C]  = {
        (es: ExecutorService) => {
            val af = parA(es)
            val bf = parB(es)
            UnitFuture(f(af.get, bf.get))
        }
    }

    def fork[A](a: => Par[A]): Par[A] = {
        es => es.submit(new Callable[A] {
            def call = a(es).get
        })
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))
}
