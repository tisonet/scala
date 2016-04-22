package tisonet.scala.steladb.sstable.index

import org.scalatest.FunSuite

class SSTableIndexSuite extends FunSuite {

    test("Size of empty index is zero"){
        assert(SSTableIndex().size == 0)
    }

    test("Add should increase index size"){
        val index = SSTableIndex().add(IndexEntry("K1", 1L)).add(IndexEntry("K2", 2L))
        assert(index.size == 2)
    }

}
