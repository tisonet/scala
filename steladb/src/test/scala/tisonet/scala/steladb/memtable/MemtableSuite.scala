package tisonet.scala.steladb.memtable

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class MemtableSuite extends FunSuite with BeforeAndAfterEach {

    val maxSize = 5
    var table: Memtable = _

    override def beforeEach() {
        table = new Memtable(maxSize)
    }

    test("Should return added value") {
        table = table.add(MemtableEntry("key1", "some data"))

        assert(table.get("key1").get.data == "some data")
    }

    test("Should not be full when size does not exceed max size") {
        table =  table.add(MemtableEntry("key1", "some data"))

        assert(!table.isFull)
    }

    test("Should be full when size reach max size") {
        table = (1 to maxSize).foldLeft(table) ((t, i) => t.add(MemtableEntry("key" + i, "some data")))

        assert(table.isFull)
    }

    test("Should not be full when only added the same key") {
        table = (1 to maxSize).foldLeft(table) ((t, i) => t.add(MemtableEntry("key", "some data")))

        assert(!table.isFull)
    }

    test("Should return entries ordered by key") {
        val entries = table
            .add(MemtableEntry("b", "b"))
            .add(MemtableEntry("c", "c"))
            .add(MemtableEntry("a", "a"))
            .entries
            .toList

        assert(entries(0) == MemtableEntry("a", "a"))
        assert(entries(1) == MemtableEntry("b", "b"))
        assert(entries(2) == MemtableEntry("c", "c"))
    }
}
