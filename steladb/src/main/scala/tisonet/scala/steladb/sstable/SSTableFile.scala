package tisonet.scala.steladb.sstable

import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets.UTF_8

object SSTableFile {
    def apply (filePath: String, position: Int = 0) = new SSTableFile(filePath, position)

    def getSize (data: String) = getBytes(data).length
    def getData(data: Array[Byte]) = new String(data, UTF_8)
    def getBytes(data: String) = data.getBytes(UTF_8)
}

/**
  * Immutable SSTable file
  *
  * @param filePath path to SSTable file
  * @param offset position in SSTable file - read, write happens from a given position
  */
class SSTableFile(val filePath: String, val offset: Int) {
    val NEW_LINE_DELIMITER = '\n'

    private def useFile (action: RandomAccessFile => Int)  =  {
        val rFile = new RandomAccessFile(filePath, "rw")
        rFile.seek(offset)
        val shift = action(rFile)
        rFile.close()

        SSTableFile(filePath, offset + shift)
    }

    def getSize(data: String) = SSTableFile.getSize(data)

    def writeLine(data: String): SSTableFile = {
        write(data + NEW_LINE_DELIMITER)
    }

    def write(data: String): SSTableFile = {

        useFile(file => {
            val bytesToWrite = SSTableFile.getBytes(data)
            file.write(bytesToWrite)
            bytesToWrite.length
        })
    }

    def read(size: Int): (String, SSTableFile) = {
        val buffer = Array.fill[Byte](size)(0)

        val sstableFile = useFile(file => {
            file.read(buffer)
        })

        (SSTableFile.getData(buffer), sstableFile)
    }
}

