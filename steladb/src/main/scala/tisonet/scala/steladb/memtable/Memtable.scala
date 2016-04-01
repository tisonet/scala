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

    def get(key: String): Option[MemtableEntry] = {

        table.get(key) match {
            case Some(data) => Some(MemtableEntry(key, data))
            case _ => None
        }
    }

    def sortedEntries = {
        for ((key, data) <- table.toSeq.sortBy(_._1))
            yield MemtableEntry(key, data)
    }

    def isFull: Boolean = table.size >= maxSize
}

