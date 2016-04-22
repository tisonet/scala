package tisonet.scala.steladb.sstable.index

import org.scalatest.FunSuite
import tisonet.scala.steladb.sstable.IOSuite
import tisonet.scala.steladb.stubs.SSTableInMemoryStorage

class SSTableIndexReaderSuite extends FunSuite with IOSuite {

    test("Should load SSTable index from storage") {

        val indexRawData = "==METADATA==\ndata:offset:000085:size:000068\nindex:offset:000163:size:000017\n" +
            "==DATA==\n000016:key1:some data\n000017:key2:other data\n000017:key3:other data\n" +
            "==INDEX==\nkey1:85\nkey3:130"

        val storage = new SSTableInMemoryStorage(indexRawData.toList)
        val expectedIndex = SSTableIndex()
            .add(IndexEntry("key1", 85))
            .add(IndexEntry("key3", 130))

        assert(SSTableIndexReader("file_path", _ => storage).read() == expectedIndex)
    }

}
