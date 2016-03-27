package tisonet.scala.steladb.sstable

import java.io.File

import scala.io.Source
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import tisonet.scala.steladb.memtable.{ImmutableMemtable, Memtable}

class SSTableSuite extends FunSuite with BeforeAndAfterEach {
    val indexSize = 2
    var sstable: SSTable = _
    var memtable: Memtable = _
    var fileName: String = _
    val filePath = "/tmp/test_sstable"

    override def beforeEach() {
        memtable = new ImmutableMemtable()

        sstable = new LocalFileSSTable(memtable, filePath, indexSize)
    }

    override def afterEach() {
        new File(fileName).delete()
    }

    test("Should flush data to file storage"){
        memtable.add("key1", "some data")
        memtable.add("key2", "other data")

        fileName = sstable.flushToStorage()

        assert(readFile(fileName) == "000016:key1:some data\n000017:key2:other data\n")
    }

    def readFile (path: String): String = Source.fromFile(path, "UTF-8").mkString
}
