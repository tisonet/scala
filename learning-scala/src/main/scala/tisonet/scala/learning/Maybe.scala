package tisonet.scala.learning

/**
  * Covariant generic class
  */
abstract class Maybe[+A] {
    def isEmpty: Boolean
    def get: A


    /* Not compile - covariant type A occurs in contravariant position in type => A of value default

     def getOrElse(default: A): A = {
        if(isEmpty) default else get
    }
    */

    // Lower bound sets the lower limit of the type parameter.
    // Type parameter T is constrained to some supper type of type A.
    def getOrElse[B >: A](default: B): B = {
        if(isEmpty) default else get
    }

}

final case class Just[A] (value: A) extends Maybe[A]{
    override def isEmpty: Boolean = false
    override def get: A = value
}

case object Nil extends Maybe[Nothing] {
    override def isEmpty: Boolean = true
    override def get: Nothing = throw new NoSuchElementException("Nil.get")
}


object MaybeUtils{
    // Upper bound function, A has to be some subclass of Maybe[_]
    def defaultToNull [A <: Maybe[_]] (p: A) = p.getOrElse(null)
}