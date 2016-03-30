package tisonet.scala.steladb.sstable

import tisonet.scala.steladb.memtable.{MemtableEntry, Memtable}

class SSTable(val memtable: Memtable, val filePath: String, val maxIndexSize: Int) {
    val NEW_LINE_DELIMITER = '\n'
    val ENTRIES_DELIMITER = ':'

    def flushToStorage(): String = {

        val fileName = filePath + System.currentTimeMillis

        var dataSize = 0L
        var dataOffset = 0L
        var indexOffset = 0L
        var indexSize = 0L

        try {
            var sstableFile = writeMetadata(SSTableMetadata(0, 0, 0, 0), SSTableFile(fileName))

            sstableFile = writeDataSection(sstableFile)
            dataOffset = sstableFile.offset

            val (sstableIndex, _sstableFile) = memtable.sortedEntries.foldLeft(new SSTableIndex, sstableFile) {
                case ((index, file), MemtableEntry(key, data)) =>
                    (index.add(IndexEntry(key, file.offset)), file.write(getFileDataForEntry((key, data)))
                )
            }

            dataSize = _sstableFile.offset - dataOffset

            sstableFile = writeIndexSection(_sstableFile)
            indexOffset = sstableFile.offset

            sstableFile = sstableIndex.sortedEntries.zipWithIndex.foldLeft(sstableFile) {
                case (file, (IndexEntry(rowKey, offset), i)) if shouldStoreEntryWithPosition(i, sstableIndex.size) =>
                    file.writeLine(rowKey + ENTRIES_DELIMITER + offset)
                case  (file, _) => file
            }

            indexSize = sstableFile.offset - indexOffset

            // Metadata update with correct offsets and sizes
            writeMetadata(
                SSTableMetadata(dataOffset, dataSize, indexOffset, indexSize),
                SSTableFile(fileName))
        }

        fileName
    }

    private def writeMetadata (metadata: SSTableMetadata, sstableFile: SSTableFile) = {
        sstableFile.writeLine("==METADATA==")
            .writeLine("data:offset:%s:size:%s" format(formatSize(metadata.dataOffset), formatSize(metadata.dataSize)))
            .writeLine("index:offset:%s:size:%s" format(formatSize(metadata.indexOffset), formatSize(metadata.indexSize)))
    }


    private def formatSize(size: Long) = "%06d".format(size)

    private def writeDataSection(sstableFile: SSTableFile) = sstableFile.writeLine("==DATA==")

    private def writeIndexSection(sstableFile: SSTableFile) = sstableFile.writeLine("==INDEX==")

    private def shouldStoreEntryWithPosition(indexEntryPosition: Int, indexSize: Int) =
        (indexEntryPosition % Math.ceil(indexSize / maxIndexSize.toDouble)) == 0

    private def getFileDataForEntry(entry: (String, String)): String = {
        val data = ENTRIES_DELIMITER + entry._1 +
            ENTRIES_DELIMITER + entry._2 + NEW_LINE_DELIMITER

        formatSize(SSTableFile.getSize(data)) + data
    }

}
