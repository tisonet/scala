package tisonet.scala.steladb.memtable

import scala.collection.immutable._

object Memtable {
    def apply() = new Memtable(10)
}

class Memtable private(val maxSize: Int, val table: Map[String, String]) {
    def this(maxSize: Int) = this(maxSize, Map())

    def add(entry: MemtableEntry): Memtable = {
        new Memtable(maxSize, table.+((entry.key, entry.data)))
    }

    def get(key: String): Option[String] = {
        table.get(key)
    }

    def sortedEntries = {
        for ((key, data) <- table.toSeq.sortBy(_._1))
            yield MemtableEntry(key, data)
    }

    def isFull: Boolean = table.size >= maxSize
}

case class MemtableEntry(val key: String, val data: String) {

}