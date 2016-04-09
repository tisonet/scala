package tisonet.scala.steladb.sstable

class SSTableReader(filePath: String) extends SSTableCommon {
    def readSSTable(): SSTable = {
        val index = readAndBuildIndex(filePath)
        new SSTable(filePath, index)
    }

    private def readAndBuildIndex(filePath: String) = {

        readIndexDataFromFile(filePath)
            .split(NEW_LINE_DELIMITER)
            .map(parseIndexEntryFromRawLine)
            .foldLeft(SSTableIndex()) {
                case (index, indexEntry) => index.add(indexEntry)
            }
    }

    private def readIndexDataFromFile(filePath: String) = {
        val metadata = SSTableMetadata(SSTableFile(filePath))
        SSTableFile(filePath, metadata.indexOffset).read(metadata.indexSize)._1
    }

    private def parseIndexEntryFromRawLine(line: String): IndexEntry = {
        val parts = line.split(ENTRIES_DELIMITER)
        IndexEntry(parts(0), parts(1).toLong)
    }
}
