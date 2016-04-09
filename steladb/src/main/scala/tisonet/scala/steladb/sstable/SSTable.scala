package tisonet.scala.steladb.sstable

import tisonet.scala.steladb.sstable.DataSizeFormater.parseSize

class SSTable(filePath: String, index: SSTableIndex) extends SSTableCommon {

    def get(rowKey: String): Option[String] = {

        def indexEntryByKey(entry: IndexEntry) = {
            entry.rowKey >= rowKey
        }

        index.sortedEntries.find(indexEntryByKey) match {
            case Some(e) => findData(e)
            case _ => None
        }
    }

    private def findData(indexEntry: IndexEntry): Option[String] = {

        def hasWantedKey(data: String): Boolean = {
            data.startsWith(indexEntry.rowKey)
        }

        def readNextEntry(offset: Long): Option[String] = {
            findData(IndexEntry(indexEntry.rowKey, offset))
        }

        readDataEntryFromSSTable(indexEntry.offset) match {
            case (Some(entry), _) if hasWantedKey(entry) => Some(parseData(entry))
            case (_, offset) => readNextEntry(offset)
            case _ => None
        }
    }

    private def readDataEntryFromSSTable(offset: Long) = {

        SSTableFile(filePath, offset).read(6) match {
            case (dataSize, sstableFile) =>
                sstableFile.read(parseSize(dataSize)) match {
                    case (data, file) => (Some(data), file.offset)
                    case _ => (None, 0L)
                }
        }
    }

    private def parseData(line: String): String = {
        line.split(ENTRIES_DELIMITER)(1)
    }
}
