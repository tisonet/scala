package tisonet.scala.steladb.sstable

import tisonet.scala.steladb.memtable.Memtable

import scala.collection.mutable


class SSTable(val memtable: Memtable, val filePath: String, val maxIndexSize: Int) {
    val NEW_LINE_DELIMITER = '\n'
    val ENTRIES_DELIMITER = ':'

    def flushToStorage(): String = {

        val index = mutable.Map[String, Long]()
        val fileName = filePath + System.currentTimeMillis
        val sstableFile: SSTableFile = new SSTableFile(fileName)

        var offset = 0L
        var dataSize = 0L
        var dataOffset = 0L
        var indexOffset = 0L
        var indexSize = 0L

        try {
            // Metadata section
            shiftOffset(writeMetadata)

            // Data section
            shiftOffset(writeDataSection)
            dataOffset = offset
            for (entry <- sortedMemtableEntries) {
                shiftOffset(writeDataEntryAndUpdateIndex(entry))
            }
            dataSize = offset - dataOffset

            // Index section
            shiftOffset(writeIndexSection)
            indexOffset = offset
            for ((entry, position) <- sortedIndexEntries.zipWithIndex) {
                if (shouldStoreEntryWithPosition(position)) {
                    shiftOffset(writeIndexEntry(entry))
                }
            }
            indexSize = offset - indexOffset

            // Metadata update with correct offsets and sizes
            seekToBeginningOfSstableFile()
            writeMetadata
        }
        finally {
            sstableFile.close()
        }

        def writeDataEntryAndUpdateIndex(entry: (String, String)): Long = {
            index.update(entry._1, offset)
            sstableFile.write(getFileDataForEntry(entry))
        }


         def getFileDataForEntry(entry: (String, String)): String = {
            val data = ENTRIES_DELIMITER + entry._1 + ENTRIES_DELIMITER + entry._2 + NEW_LINE_DELIMITER
            val dataSize = sstableFile.getSize(data)
            formatSize(dataSize) + data
        }

        def writeIndexEntry(entry: (String, Long)): Long = {
            sstableFile.writeLine(entry._1 + ENTRIES_DELIMITER + entry._2)
        }

        def shiftOffset(shiftSize: Long) = offset += shiftSize

        def writeDataSection: Long = sstableFile.writeLine("==DATA==")

        def writeIndexSection: Long = sstableFile.writeLine("==INDEX==")

        def writeMetadata: Long = {
            sstableFile.writeLine("==METADATA==") +
                sstableFile.writeLine("data:offset:%s:size:%s" format(formatSize(dataOffset), formatSize(dataSize))) +
                sstableFile.writeLine("index:offset:%s:size:%s" format(formatSize(indexOffset), formatSize(indexSize)))
        }

        def shouldStoreEntryWithPosition(indexEntryPosition: Int) =
            (indexEntryPosition % Math.ceil(index.size / maxIndexSize.toDouble)) == 0

        def sortedIndexEntries: Seq[(String, Long)] = {
            index.toSeq.sortBy(_._1)
        }
        def seekToBeginningOfSstableFile(): Unit = sstableFile.seek(0)

        fileName
    }

    private def sortedMemtableEntries = memtable.getAll.sortBy(_._1)

    private def formatSize(size: Long) = "%06d".format(size)
}
