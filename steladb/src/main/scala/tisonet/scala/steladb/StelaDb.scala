package tisonet.scala.steladb

import tisonet.scala.steladb.OpType._

class StelaDb(private val memtable: Memtable, private val commitLog: CommitLog) {
    var logId = 0L

    def write(key: String, data: String) = {

        logId = commitLog.log(Op(Write, key, Some(data)))

        memtable.add(key, data)

        if (memtable.isFull) {
            flush
        }

    }

    private def flush = ???

    def read(key: String): Option[String] = {
        logId = commitLog.log(Op(Read, key, None))

        memtable.get(key)
    }

    def remove(key: String) = {
        logId = commitLog.log(Op(Remove, key, None))
    }
}
