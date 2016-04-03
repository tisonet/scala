package tisonet.scala.steladb.sstable

import tisonet.scala.steladb.sstable.DataSizeFormater.parseSize

class SSTable(filePath: String, index: SSTableIndex) extends SSTableCommon{

    def get(rowKey: String): Option[String] = {
        index.sortedEntries.find { case IndexEntry(key, _) => key >= rowKey } match {
            case Some(e) => findData(e)
            case _ => None
        }
    }

    private def findData(entry: IndexEntry): Option[String] = {

       readDataFromSSTable(entry.offset) match {
           case (Some(data), _) if data.startsWith(entry.rowKey) => Some(parseData(data))
           case (_, offset) => findData(IndexEntry(entry.rowKey, offset))
           case _ => None
        }

    }

    private def readDataFromSSTable(offset: Long) = {

        SSTableFile(filePath, offset).read(6) match {
            case (dataSize, sstableFile) =>
                sstableFile.read(parseSize(dataSize)) match {
                    case (data, file) => (Some(data), file.offset)
                    case _ => (None, 0L)
                }
        }
    }

    private def parseData (line: String): String = {
        line.split(ENTRIES_DELIMITER) match {
            case (key, data) => data
        }
    }
}
