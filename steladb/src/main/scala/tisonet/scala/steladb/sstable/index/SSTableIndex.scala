package tisonet.scala.steladb.sstable.index

import scala.Predef.Map
import scala.collection.immutable._

object SSTableIndex {
    def apply(): SSTableIndex = new SSTableIndex(Map())
}

class SSTableIndex private (val index: Map[String, Long]) {

    def add(entry: IndexEntry) = {
        new SSTableIndex(index.+((entry.rowKey, entry.offset)))
    }

    def sortedEntries = {
        for ((key, offset) <- index.toSeq.sortBy(_._1))
            yield IndexEntry(key, offset)
    }

    def getOffset(key: String) = {
        // TODO: Binary search the closest index
        def indexEntryByKey(entry: IndexEntry) = {
            entry.rowKey.compare(key) >= 0
        }

        sortedEntries.find(indexEntryByKey)
    }

    def size = index.size

    override def equals(obj: Any) = {
        obj.isInstanceOf[SSTableIndex] && obj.asInstanceOf[SSTableIndex].index == index
    }
}

