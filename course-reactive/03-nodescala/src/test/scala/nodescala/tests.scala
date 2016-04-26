package nodescala

import nodescala.NodeScala._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.postfixOps
import ExecutionContext.Implicits.global

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be completed") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }
  test("A Future should never be completed") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
    test("Future should completed when all futures complete"){
        val all = Future.all(List(Future.always(5), Future.always(6), Future.always(7)))
        assert(Await.result(all, 1 second) == List(5, 6, 7))
    }

    test("Future should fail when any of all fails"){
        val all = Future.all(List(Future.always(5), Future.failed(new Exception), Future.always(7)))
        try {
            Await.result(all, 1 second)
            assert(false)
        } catch {
            case t: Exception => // ok!
        }
    }

    test ("Should return value of future which finish first") {
        val any = Future.any(List(
            Future.delayedValue(6 milliseconds, 6),
            Future.delayedValue(2 milliseconds, 2),
            Future.delayedValue(3 milliseconds, 3))
        )

        val results = Await.result(any, 1 second)
        assert(results == 2)
    }


  test ("now should return throws exception when future not finished") {
    try {
      Future.delayedValue(6 milliseconds, 6).now
      assert(false)
    } catch {
      case t: NoSuchElementException =>
      case t: Exception => assert(false)
    }
  }

  test ("now should return value when future is finished") {
    assert(Future.always(3).now == 3)
  }

  test ("continueWith should continue with a given function and returns results of a given function"){
    val f = Future.always(1).continueWith(f => f.now * 2)
    val results = Await.result(f, 10 millis)
    assert(results == 2)
  }

  test ("continue should continue with a given function and returns results of a given function"){
    val f = Future.always(1).continue(f => f.get * 2)
    val results = Await.result(f, 10 millis)
    assert(results == 2)
  }

  test ("run can be canceled") {
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          println("working")
        }
        assert(true)
        println("done")
      }
    }

    val f = Future.delay(5 millisecond)

    f onSuccess {
      case _ => working.unsubscribe()
    }
  }




  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }
  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




