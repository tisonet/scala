package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength
import calculator.Calculator.computeValues

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }


  test("a = 1") {
    assert(computeValues(Map(
      e("a", Literal(1)))).get("a").get() == 1)
  }

  test("a = 1 + 2") {
    assert(computeValues(Map(
     e("a", Plus(Literal(1), Literal(2))))).get("a").get() == 3)
  }

  test("a = 5 - 2") {
    assert(computeValues(Map(
     e("a", Minus(Literal(5), Literal(2))))).get("a").get() == 3)
  }

  test("a = 5 * 2") {
    assert(computeValues(Map(
     e("a", Times(Literal(5), Literal(2))))).get("a").get() == 10)
  }

  test("a = 10 / 5") {
    assert(computeValues(Map(
     e("a", Divide(Literal(10), Literal(5))))).get("a").get() == 2)
  }

  test("a = 1; b = a") {
    assert(computeValues(Map(
     e("a", Literal(1)), e("b", Ref("a")))).get("b").get() == 1)
  }

  test("a = 1; b = 2 * a") {
    assert(computeValues(Map(
      e("a", Literal(1)), e("b", Times(Literal(2), Ref("a"))))).get("b").get() == 2)
  }

  test("b = 2 * z") {
    assert(computeValues(Map(
      e("b", Times(Literal(2), Ref("z"))))).get("b").get().isNaN)
  }

  test("a = b; b = a + 1") {
    assert(computeValues(Map(
      e("a", Ref("b")),
      e("b", Plus(Ref("a"), Literal(1))))).get("b").get().isNaN)
  }

  test("a = c; b = c + a; c = b") {
    assert(computeValues(Map(
      e("a", Ref("c")),
      e("b", Plus(Ref("c"), Ref("a"))),
      e("c",  Ref("b"))
    )).get("c").get().isNaN)
  }

  def e (name: String, expr: Expr) = {
    (name, Signal(expr))
  }
}
