package tisonet.scala.steladb.sstable

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class SSTableIndexReaderSuite extends FunSuite with BeforeAndAfterEach with IOSuite {
    var filePath: String = _

    override def beforeEach() {
        filePath = testFilePath
        dumpIndexData()
    }

    test("Should load SSTable correctly from file") {
        val index = SSTableIndexReader(filePath).read()
        assert(index != null)
    }

    private def dumpIndexData() = {
        val indexData = "==METADATA==\ndata:offset:000085:size:000068\nindex:offset:000163:size:000017\n" +
            "==DATA==\n000016:key1:some data\n000017:key2:other data\n000017:key3:other data\n" +
            "==INDEX==\nkey1:85\nkey3:130"

        writeToFile(filePath, indexData)
    }
}
