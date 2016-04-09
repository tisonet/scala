package tisonet.scala.steladb.memtable

import scala.collection.immutable._

object Memtable {
    def apply() = new Memtable(10)
}

class Memtable private(val maxSize: Int, val table: SortedMap[String, String]) {
    def this(maxSize: Int) = this(maxSize, TreeMap())

    def add(entry: MemtableEntry) = {
        new Memtable(maxSize, table.+((entry.key, entry.data)))
    }

    def get(key: String): Option[MemtableEntry] = {
        table.get(key) match {
            case Some(data) => Some(MemtableEntry(key, data))
            case _ => None
        }
    }

    def entries = {
        for ((key, data) <- table)
            yield MemtableEntry(key, data)
    }

    def isFull = table.size >= maxSize
}

