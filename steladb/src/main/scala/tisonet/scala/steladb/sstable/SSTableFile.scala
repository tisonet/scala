package tisonet.scala.steladb.sstable

import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets._

class SSTableFile(val filePath: String) {
    val NEW_LINE_DELIMITER = '\n'

    lazy val file = new RandomAccessFile(filePath, "rw")

    def getSize(data: String) = getBytes(data).length

    def writeLine(data: String): Long = {
        write(data + NEW_LINE_DELIMITER)
    }

    def write(data: String): Long = {
        val bytesToWrite = getBytes(data)
        file.write(bytesToWrite)
        bytesToWrite.length
    }

    def seek(position: Long) = file.seek(position)

    def read(size: Int): String = {
        val buffer = Array.fill[Byte](size)(0)
        file.read(buffer)
        getData(buffer)
    }

    def close(): Unit = file.close()

    private def getData(data: Array[Byte]) = new String(data, UTF_8)

    private def getBytes(data: String) = data.getBytes(UTF_8)
}

