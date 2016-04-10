package tisonet.scala.steladb.stubs

import tisonet.scala.steladb.sstable.SSTableStorage

class SSTableInMemoryStorage(var buffer: List[Char] = List()) extends SSTableStorage {
    val filePath: String = ""
    var currentOffset = 0L

    def getSize(data: String): Long = SSTableStorage.getSize(data)

    def write(data: String): SSTableStorage = {
        if (currentOffset >= buffer.size) {
            buffer = buffer ++ data
            currentOffset = buffer.size

        }else{
            buffer = buffer.slice(0, currentOffset.toInt) ++
                data ++
                buffer.slice(currentOffset.toInt + data.length, buffer.length)
            currentOffset = currentOffset.toInt + data.length
        }

        this
    }

    def writeLine(data: String): SSTableStorage = write(data + '\n')

    def read(size: Long): (String, SSTableStorage) = {
        val data = buffer.slice(offset.toInt, offset.toInt + size.toInt).mkString
        currentOffset = currentOffset + size
        (data, this)
    }

     def offset = currentOffset
}
