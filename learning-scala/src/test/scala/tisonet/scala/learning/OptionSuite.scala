package tisonet.scala.learning

import org.scalatest.FunSuite

class OptionSuite extends FunSuite{
    test("Should map Some to Some") {
        assert(Some("a").map(x => x) == Some("a"))
    }

    test("Should map Some to another Some") {
        assert(Some("a").map(x => "b") == Some("b"))
    }

    test("Should map None to None") {
        assert(None.map(x => "b") == None)
    }

    test("Should flatMap Some to Some") {
        assert(Some("a").flatMap(x => Some(x)) == Some("a"))
    }

    test("Should flatMap Some to another Some") {
        assert(Some("a").flatMap(x =>  Some("b")) == Some("b"))
    }

    test("Should flatMap None to None") {
        assert(None.flatMap(x => Some("b")) == None)
    }

    test("Should flatMap Some to None") {
        assert(Some("a").flatMap(x => None) == None)
    }

    test("Should get value") {
        assert(Some("a").getOrElse("b") == "a")
    }

    test("Should get default value") {
        assert(None.getOrElse("b") == "b")
    }

    test("Should else value when Some") {
        assert(Some("a").orElse(Some("b")) == Some("a"))
    }

    test("Should else Some when Some") {
        assert(None.orElse(Some("b")) == Some("b"))
    }

    test("Should return Some when filter passes") {
        assert(Some("a").filter( x => x == "a" ) == Some("a"))
    }

    test("Should return None when filter fails") {
        assert(Some("a").filter(x => x != "a" ) == None)
    }
}
