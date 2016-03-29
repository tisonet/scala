package tisonet.scala.steladb.sstable

import java.io.File

import scala.io.Source
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import tisonet.scala.steladb.memtable.Memtable

class SSTableSuite extends FunSuite with BeforeAndAfterEach {
    val indexSize = 2
    var sstable: SSTable = _
    var memtable: Memtable = _
    var fileName: String = _
    val filePath = "test_sstable"

    override def beforeEach() {
        memtable = new Memtable()

        sstable = new SSTable (memtable, filePath, indexSize)
    }

    override def afterEach() {
        new File(fileName).delete()
    }

    test("Should flush data to file storage"){
        memtable.add("key1", "some data")
        memtable.add("key2", "other data")
        memtable.add("key3", "other data")

        val expected = "" +
            "==METADATA==\ndata:offset:000085:size:000068\nindex:offset:000163:size:000017\n" +
            "==DATA==\n000016:key1:some data\n000017:key2:other data\n000017:key3:other data\n" +
            "==INDEX==\nkey1:85\nkey3:130\n"

        fileName = sstable.flushToStorage()

        assert(readFile(fileName) == expected)
    }

    def readFile (path: String): String = Source.fromFile(path, "UTF-8").mkString
}
