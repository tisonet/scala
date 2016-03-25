package tisonet.scala.learning.collections

import org.scalatest.FunSuite
import tisonet.scala.learning.collections.HighOrderFunctions._

class HighOrderFunctionsSuite extends FunSuite {

    def addOne (x: Int) = x + 1

    test("Should map list to new list with applied function") {
        assert(map(List(5, 3, 7), addOne) == List(6, 4, 8))
    }

    test("Should map empty list") {
        assert(map(List(), addOne) == List())
    }

    test("Should map list to new list with applied function 2") {
        assert(map2(List(5, 3, 7), addOne) == List(6, 4, 8))
    }

    test("Should map empty list 2") {
        assert(map2(List(), addOne) == List())
    }

    test("Should flatten collection of lists") {
        assert(flatten(List("Hello", "world", "!").map(_.toList))
            == List('H', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd', '!'))
    }

    test("Should flatten empty collection"){
        assert(flatten(List()) == List())
    }

    test("Should flatten collection with one element"){
        assert(flatten(List(List("Hello"))) == List("Hello"))
    }

    test("Should flat map") {
        assert(flatMap(List("Hello", "world", "!")) { _.toList } ==
            List('H', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd', '!'))
    }
}
