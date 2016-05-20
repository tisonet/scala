package tisonet.scala.learning.state


// Generates int and return generator
trait RNG {
    def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
            val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
            val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
            (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
        }
    }

    // We need to be quite careful not to skew the generator.
    // Since `Int.Minvalue` is 1 smaller than `-(Int.MaxValue)`,
    // it suffices to increment the negative numbers by 1 and make them positive.
    // This maps Int.MinValue to Int.MaxValue and -1 to 0.
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        (if (i < 0) -(i + 1) else i, r)
    }

    def ints(count: Int): (List[Int], RNG) = {
        (1 to count).foldRight((List[Int](), new Simple(0L): RNG)) {
            (_, pair) => {
                val (newInt, newGen) = pair._2.nextInt
                (newInt :: pair._1, newGen)
            }
        }
    }

    // Uses RNG to generates a new A and transitions RNG to a new state.
    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    // Returns function which returns constant and transfer RNG state
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    // Returns function which takes RNG, generates value, maps it by function f
    // and returns value with new RNG state.
    def map[A,B](s: Rand[A]) (f: A => B): Rand[B] = {
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }
    }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, rngA) = ra(rng)
            val (b, rngB) = rb(rngA)
            (f(a,b), rngB)
        }
    }

    def nonNegativeLessThan(n: Int) : Rand[Int] = map(nonNegativeInt) {a => a % n}

    def flatMap[A,B](f: Rand[A]) (g: A => Rand[B]): Rand[B] = {
        rng => {
            val (a, rngA) = f(rng)
            g(a)(rngA)  // We pass the new state along
        }
    }
}


