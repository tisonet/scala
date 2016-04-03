package tisonet.scala.steladb.sstable

object DataSizeFormater {
    def formatSize(size: Long) = "%06d".format(size)
    def parseSize(formatedSize: String) = formatedSize.toLong
}
