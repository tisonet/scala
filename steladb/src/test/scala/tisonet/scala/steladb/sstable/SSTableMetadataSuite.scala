package tisonet.scala.steladb.sstable

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class SSTableMetadataSuite extends FunSuite with BeforeAndAfterEach {

    override def beforeEach() {
    }

    override def afterEach() {

    }

    test("Should parse metadata correctly") {
        val metadataText = "==METADATA==\ndata:offset:000085:size:000068\nindex:offset:000163:size:000017"
        val metadata = SSTableMetadata().parseMetadata(metadataText)
        assert(metadata == SSTableMetadata(85, 68, 163, 17))
    }

}
