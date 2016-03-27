package tisonet.scala.steladb.sstable

import tisonet.scala.steladb.memtable.Memtable

trait SSTable {
    val memtable: Memtable

    def flushToStorage(): String
}