package tisonet.scala.steladb.memtable

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class MemtableSuite extends FunSuite with BeforeAndAfterEach {

    val maxSize = 5
    var table: Memtable = _

    override def beforeEach() {
        table = new Memtable(maxSize)
    }

    test("Should return added value") {
        table.add("key1", "some data")

        assert(table.get("key1").get == "some data")
    }

    test("Should not be full when size does not exceed max size") {
        table.add("key1", "some data")

        assert(!table.isFull)
    }

    test("Should be full when size reach max size") {
        (1 to maxSize).foreach(i => table.add("key" + i, "some data"))

        assert(table.isFull)
    }

    test("Should be full when added the same key") {
        (1 to maxSize) foreach (_ => table.add("key", "some data"))

        assert(!table.isFull)
    }
}
