package tisonet.scala.steladb.sstable

import java.io._
import java.nio.charset.StandardCharsets.UTF_8

import tisonet.scala.steladb.memtable.Memtable

import scala.collection.mutable


class SSTable(val memtable: Memtable, val filePath: String, val maxIndexSize: Int) {

    val NEW_LINE_DELIMITER = '\n'
    val ENTRIES_DELIMITER = ':'

    def flushToStorage(): String = {

        val index = mutable.Map[String, Long]()
        var sstableFile: RandomAccessFile = null
        val fileName = filePath + System.currentTimeMillis

        var offset = 0L
        var dataSize = 0L
        var dataOffset = 0L
        var indexOffset = 0L
        var indexSize = 0L

        try {
            sstableFile = openFile(fileName)

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
            writeToSSTable(getFileDataForEntry(entry), sstableFile)
        }

        def writeIndexEntry(entry: (String, Long)): Long = {
            writeLineToSSTable(entry._1 + ENTRIES_DELIMITER + entry._2, sstableFile)
        }

        def shiftOffset(shiftSize: Long) = offset += shiftSize

        def writeDataSection: Long = writeLineToSSTable("==DATA==", sstableFile)

        def writeIndexSection: Long = writeLineToSSTable("==INDEX==", sstableFile)

        def writeMetadata: Long = {
            writeLineToSSTable("==METADATA==", sstableFile) +
                writeLineToSSTable("data:offset:%s:size:%s" format(formatSize(dataOffset), formatSize(dataSize)), sstableFile) +
                writeLineToSSTable("index:offset:%s:size:%s" format(formatSize(indexOffset), formatSize(indexSize)), sstableFile)
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

    private def writeLineToSSTable(data: String, sstableFile: RandomAccessFile): Long = {
        writeToSSTable(data + NEW_LINE_DELIMITER, sstableFile)
    }

    private def writeToSSTable(data: String, sstableFile: RandomAccessFile): Long = {
        val bytesToWrite = getBytes(data)
        sstableFile.write(bytesToWrite)
        bytesToWrite.length
    }

    private def getFileDataForEntry(entry: (String, String)): String = {
        val data = ENTRIES_DELIMITER + entry._1 + ENTRIES_DELIMITER + entry._2 + NEW_LINE_DELIMITER
        val dataSize = getBytes(data).length
        formatSize(dataSize) + data
    }

    private def formatSize(size: Long) = "%06d".format(size)

    private def getBytes(data: String) = data.getBytes(UTF_8)

    private def openFile(fileName: String) = new RandomAccessFile(fileName, "rw")


}
