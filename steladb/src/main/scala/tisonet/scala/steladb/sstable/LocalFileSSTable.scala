package tisonet.scala.steladb.sstable

import java.io._
import java.nio.charset.StandardCharsets.UTF_8

import tisonet.scala.steladb.memtable.Memtable

import scala.collection.mutable


class LocalFileSSTable(override val memtable: Memtable, val filePath: String, val indexSize: Int)
        extends SSTable {

    val metadataSize = getBytes(formatSize(0)).length

    override def flushToStorage(): String = {

        val index = mutable.Map[String, Long]()
        val offset = 0L
        var sstableFile: BufferedWriter = null
        val fileName = filePath + System.currentTimeMillis

        try {
            sstableFile = openFile(fileName)

            for (entry <- sortedEntries) {
                writeEntryToFileAndUpdateIndex(entry)
            }
        }
        finally
            sstableFile close()

        def writeEntryToFileAndUpdateIndex(entry: (String, String)): Long = {
            index.update(entry._1, offset)

            val fileEntry = getFileDataForEntry(entry)
            sstableFile.write(fileEntry)

            offset + getBytes(fileEntry).length
        }

        fileName
    }

    private def sortedEntries = memtable.getAll.sortBy(_._1)

    private def getFileDataForEntry(entry: (String, String)): String = {
        val data = ":" + entry._1 + ":" + entry._2 + "\n"
        val dataSize = getBytes(data).length

        formatSize(dataSize) + data
    }

    private def formatSize(size: Int) = "%06d".format(size)

    private def getBytes(data: String) = data.getBytes(UTF_8)

    private def openFile(fileName: String) = new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(new File(fileName)), UTF_8))

}
