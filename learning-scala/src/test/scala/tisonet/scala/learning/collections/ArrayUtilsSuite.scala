package tisonet.scala.learning.collections

import org.scalatest.FunSuite

class ArrayUtilsSuite extends FunSuite {
    test("Should swap adjacent elements") {
        val a = Array(1, 2, 3, 4, 5)
        ArrayUtils.swapAdjacent(a)
        assert(a.sameElements(Array(2, 1, 4, 3, 5)))
    }

    test("Should order by positivity") {
        assert(
            ArrayUtils.orderByPositivity(Array(-1, 2, -5, 0, 2, 5, 10)).sameElements(Array(2, 2, 5, 10, 0, -1, -5)))
    }

    test("Should calculate average") {
        assert(ArrayUtils.average(Array(5, 3, 7)) == 5.0)
    }

}
