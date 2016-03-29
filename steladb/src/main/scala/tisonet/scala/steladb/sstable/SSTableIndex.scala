package tisonet.scala.steladb.sstable

import scala.collection.immutable._

class SSTableIndex(val index: Map[String, Long]) {
    def this() = this(new HashMap[String, Long]())

    def add(entry: IndexEntry) = {
        new SSTableIndex(index.+((entry.rowKey, entry.offset)))
    }

    def sortedEntries = {
        for ((key, offset) <- index.toSeq.sortBy(_._1))
            yield IndexEntry(key, offset)
    }

    def size = index.size
}

case class IndexEntry(rowKey: String, offset: Long)
