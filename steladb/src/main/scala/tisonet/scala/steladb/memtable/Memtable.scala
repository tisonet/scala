package tisonet.scala.steladb.memtable

import scala.collection.immutable

class Memtable(val maxSize: Int = 100) {
    var table: immutable.Map[String, String] = immutable.Map()

    def add(key: String, data: String): Unit = {
        table = table.+((key, data))
    }

    def get(key: String): Option[String] = {
        table.get(key)
    }

    def getAll: List[(String, String)] = {
        table.toList
    }

    def isFull: Boolean = table.size >= maxSize
}