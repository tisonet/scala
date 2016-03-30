package tisonet.scala.steladb

import tisonet.scala.steladb.OpType._
import tisonet.scala.steladb.memtable.{MemtableEntry, Memtable}

class StelaDb(private var memtable: Memtable, private val commitLog: CommitLog) {
    var logId = 0L

    def write(key: String, data: String) = {

        logId = commitLog.log(Op(Write, key, Some(data)))

        memtable = memtable.add(MemtableEntry(key, data))

        if (memtable.isFull) {
            flush
        }

    }

    private def flush = ???

    def read(key: String): Option[String] = {
        logId = commitLog.log(Op(Read, key, None))

        memtable.get(key) match {
            case Some(MemtableEntry(_, row)) => Some(row)
            case _ => None
        }
    }

    def remove(key: String) = {
        logId = commitLog.log(Op(Remove, key, None))
    }
}
