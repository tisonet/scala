package tisonet.scala.steladb.sstable

import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets.UTF_8

trait SSTableStorage {
    def filePath: String
    def offset: Long

    def write(data: String): SSTableStorage
    def writeLine(data: String): SSTableStorage
    def read(size: Long): (String, SSTableStorage)
    def getSize(data: String): Long
}

object SSTableStorage {
    def apply (filePath: String, position: Long = 0) = new SSTableFileStorage(filePath, position)

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
class SSTableFileStorage(val filePath: String, val offset: Long) extends SSTableIO with SSTableStorage{

    private def useFile (action: RandomAccessFile => Int)  =  {
        val rFile = new RandomAccessFile(filePath, "rw")
        rFile.seek(offset)
        val shift = action(rFile)
        rFile.close()

        SSTableStorage(filePath, offset + shift)
    }

    def getSize(data: String) = SSTableStorage.getSize(data)

    def writeLine(data: String): SSTableStorage = {
        write(data + NEW_LINE_DELIMITER)
    }

    def write(data: String): SSTableStorage = {

        useFile(file => {
            val bytesToWrite = SSTableStorage.getBytes(data)
            file.write(bytesToWrite)
            bytesToWrite.length
        })
    }

    def read(size: Long): (String, SSTableStorage) = {
        val buffer = Array.fill[Byte](size.toInt)(0)

        val sstableFile = useFile(file => {
            file.read(buffer)
        })

        (SSTableStorage.getData(buffer), sstableFile)
    }
}



