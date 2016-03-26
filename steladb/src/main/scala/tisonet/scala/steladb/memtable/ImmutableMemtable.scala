package tisonet.scala.steladb.memtable

import scala.collection.immutable

class ImmutableMemtable(val maxSize: Int) extends Memtable {
    var table: immutable.Map[String, String] = immutable.Map()

    override def add(key: String, data: String): Unit = {
        table = table.+((key, data))
    }

    override def get(key: String): Option[String] = {
        table.get(key)
    }

    override def isFull: Boolean = table.size >= maxSize
}