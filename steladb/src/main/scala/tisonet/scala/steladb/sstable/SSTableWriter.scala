package tisonet.scala.steladb.sstable

import java.lang.System.currentTimeMillis

import tisonet.scala.steladb.memtable.{Memtable, MemtableEntry}

class SSTableWriter(val memtable: Memtable, val filePath: String, val maxIndexSize: Int)
    extends SSTableCommon{

    def flushToStorage(): String = {

        try {
            var metadata = SSTableMetadata()
            var sstableFile = metadata.writeToFile(SSTableFile(filePath + currentTimeMillis))

            sstableFile = writeDataSection(sstableFile)
            metadata = metadata.copy(sstableFile.offset)

            val (sstableIndex, _sstableFile) = memtable.entries.foldLeft(SSTableIndex(), sstableFile) {
                case ((index, file), MemtableEntry(key, data)) => (
                        index.add(IndexEntry(key, file.offset)),
                        file.write(getFileDataForEntry(DataEntry(key, data)))
                )
            }
            metadata = metadata.copy(dataSize = _sstableFile.offset - metadata.dataOffset)

            sstableFile = writeIndexSection(_sstableFile)
            metadata = metadata.copy(indexOffset = sstableFile.offset)

            sstableFile = sstableIndex.sortedEntries.zipWithIndex.foldLeft(sstableFile) {
                case (file, (IndexEntry(rowKey, offset), i)) if shouldStoreEntryWithPosition(i, sstableIndex.size) =>
                    file.writeLine(rowKey + ENTRIES_DELIMITER + offset)
                case  (file, _) => file
            }

            metadata = metadata.copy(indexSize = sstableFile.offset - metadata.indexOffset)

            // Metadata update with correct offsets and sizes
            metadata.writeToFile(SSTableFile(sstableFile.filePath)).filePath
        }
    }

    private def writeDataSection(sstableFile: SSTableFile) = sstableFile.writeLine("==DATA==")

    private def writeIndexSection(sstableFile: SSTableFile) = sstableFile.writeLine("==INDEX==")

    private def shouldStoreEntryWithPosition(indexEntryPosition: Int, indexSize: Int) =
        (indexEntryPosition % Math.ceil(indexSize / maxIndexSize.toDouble)) == 0

    private def getFileDataForEntry(entry: DataEntry): String = {
        val data = ENTRIES_DELIMITER + entry.key +
            ENTRIES_DELIMITER + entry.data + NEW_LINE_DELIMITER

        DataSizeFormater.formatSize(SSTableFile.getSize(data)) + data
    }

}
