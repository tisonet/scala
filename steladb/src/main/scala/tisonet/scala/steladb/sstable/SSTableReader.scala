package tisonet.scala.steladb.sstable

class SSTableReader(filePath: String) extends SSTableCommon {
    def readSSTable(): SSTable = {
        val metadata = SSTableMetadata(SSTableFile(filePath))
        val (indexData, _) = SSTableFile(filePath, metadata.indexOffset).read(metadata.indexSize)


        val sstableIndex = indexData.split(NEW_LINE_DELIMITER)
            .map(_.split(ENTRIES_DELIMITER))
            .foldLeft(SSTableIndex()) {
                case (index, (key, data)) => index.add(IndexEntry(key, data))
            }

        new SSTable(filePath, sstableIndex)
    }
}
