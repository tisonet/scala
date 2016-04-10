package tisonet.scala.steladb.sstable

object SSTableIndexReader {
    def apply(filePath: String) = new SSTableIndexReader(filePath)
}

class SSTableIndexReader private(filePath: String) extends SSTableIO {
    def read(): SSTableIndex = {
        readIndexDataFromFile(filePath)
            .split(NEW_LINE_DELIMITER)
            .map(parseIndexEntryFromRawLine)
            .foldLeft(SSTableIndex()) {
                case (index, indexEntry) => index.add(indexEntry)
            }
    }

    private def readIndexDataFromFile(filePath: String) = {
        val metadata = SSTableMetadata(SSTableStorage(filePath))
        SSTableStorage(filePath, metadata.indexOffset).read(metadata.indexSize)._1
    }

    private def parseIndexEntryFromRawLine(line: String): IndexEntry = {
        val parts = line.split(ENTRIES_DELIMITER)
        IndexEntry(parts(0), parts(1).toLong)
    }
}
