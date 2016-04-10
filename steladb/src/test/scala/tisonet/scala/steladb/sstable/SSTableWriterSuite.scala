package tisonet.scala.steladb.sstable

import org.scalatest.FunSuite
import tisonet.scala.steladb.memtable.{Memtable, MemtableEntry}
import tisonet.scala.steladb.stubs.SSTableInMemoryStorage

class SSTableWriterSuite extends FunSuite {
    val indexSize = 2
    var sstable: SSTableWriter = _
    var fileName: String = _
    val filePath = "test_sstable"


    test("Should flush data to storage"){
        val sstableStorage = new SSTableInMemoryStorage()
        val memtable = Memtable()
            .add(MemtableEntry("key1", "some data"))
            .add(MemtableEntry("key2", "other data"))
            .add(MemtableEntry("key3", "other data"))

        val expected = "" +
            "==METADATA==\ndata:offset:000085:size:000068\nindex:offset:000163:size:000017\n" +
            "==DATA==\n000016:key1:some data\n000017:key2:other data\n000017:key3:other data\n" +
            "==INDEX==\nkey1:85\nkey3:130\n"


        sstable = new SSTableWriter (memtable, filePath, indexSize,  _ => {
            sstableStorage.currentOffset = 0
            sstableStorage })
        sstable.flushToStorage()

        assert(sstableStorage.buffer.mkString == expected)
    }
}
