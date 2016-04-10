package tisonet.scala.steladb.sstable.index

import tisonet.scala.steladb.sstable.SSTableStorage.SSTableStorageFactory
import tisonet.scala.steladb.sstable.{SSTableIO, SSTableMetadata, SSTableStorage}

object SSTableIndexReader {
    def apply(filePath: String) = new SSTableIndexReader(filePath, path => SSTableStorage(path, 0))
    def apply(filePath: String, sstableStorageFactory: SSTableStorageFactory) =
        new SSTableIndexReader(filePath, sstableStorageFactory)

}

class SSTableIndexReader private(filePath: String, sstableStorageFactory: SSTableStorageFactory)
    extends SSTableIO {

    def read() = {
        readIndexDataFromFile(filePath)
            .split(NEW_LINE_DELIMITER)
            .map(parseIndexEntryFromRawLine)
            .foldLeft(SSTableIndex()) {
                case (index, indexEntry) => index.add(indexEntry)
            }
    }

    private def readIndexDataFromFile(filePath: String) = {
        val metadata = SSTableMetadata(sstableStorageFactory(filePath))
        sstableStorageFactory(filePath)
            .seek(metadata.indexOffset)
            .read(metadata.indexSize)
            ._1
    }

    private def parseIndexEntryFromRawLine(line: String) = {
        val parts = line.split(ENTRIES_DELIMITER)
        IndexEntry(parts(0), parts(1).toLong)
    }
}
