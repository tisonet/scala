package tisonet.scala.steladb.sstable

case class SSTableMetadata(dataOffset: Long, dataSize: Long, indexOffset: Long, indexSize: Long)
