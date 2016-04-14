package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minFromTwoElements") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("deleteInsertedElement") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    h == empty
  }

  property("minimumFromMelt") = forAll { (a: H, b: H) =>
    val melt = meld(a, b)
    findMin(melt) == Math.min(findMin(a), findMin(b))
  }

  property("sorted") = forAll { (a: List[Int]) =>
    val h = a.foldRight(empty) { (h, a) => insert(h, a) }

    checkHead(h, a.sorted)
  }

  private def checkHead (h: H, a: List[Int]): Boolean =  a match {
    case head::tail => (findMin(h) == head) && checkHead(deleteMin(h), tail)
    case List() => isEmpty(h)
  }


  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


}
