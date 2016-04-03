package tisonet.scala.steladb.sstable

import tisonet.scala.steladb.sstable.DataSizeFormater.formatSize
import tisonet.scala.steladb.sstable.DataSizeFormater.parseSize

object SSTableMetadata {
    def apply(sstableFile: SSTableFile) = new SSTableMetadata().loadFromFile(sstableFile)
}

case class SSTableMetadata(dataOffset: Long = 0, dataSize: Long = 0,
                           indexOffset: Long = 0, indexSize: Long = 0) {

    val METADATA_TITLE_LINE = "==METADATA==\n"
    val DATA_LINE_PATTERN = "data:offset:%s:size:%s\n"
    val INDEX_LINE_PATTERN = "index:offset:%s:size:%s\n"

    def writeToFile(sstableFile: SSTableFile) = {

        sstableFile
            .write(METADATA_TITLE_LINE)
            .write(dataLine())
            .write(indexLine())
    }

    def loadFromFile(sstableFile: SSTableFile) = {
        val metadataSize = sstableFile.getSize(METADATA_TITLE_LINE + dataLine() + indexLine())

        parseMetadata(sstableFile.read(metadataSize)._1)
    }

    private def dataLine(): String = DATA_LINE_PATTERN format(formatSize(dataOffset), formatSize(dataSize))

    private def indexLine(): String = INDEX_LINE_PATTERN format(formatSize(indexOffset), formatSize(indexSize))

    def parseMetadata(metadata: String): SSTableMetadata = {
        val Parser = """==METADATA==\ndata:offset:([0-9]+):size:([0-9]+)\nindex:offset:([0-9]+):size:([0-9]+)""".r

        metadata match {
            case Parser(dOffset, dSize, iOffset, iSize) => new SSTableMetadata(
                parseSize(dOffset), parseSize(dSize), parseSize(iOffset), parseSize(iSize))
            case _ => SSTableMetadata()
        }
    }
}
